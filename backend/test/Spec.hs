{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Data.Aeson (Value (Array, Null), eitherDecode, object, toJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, Value (Object), parseEither)
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Map.Strict qualified as Map
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Database.SQLite.Simple (Connection, Only (..), execute_, query_, withConnection)
import Limits (Limit (..), rememberAt, seenAt, spend)
import Network.HTTP.Simple (parseRequest_, setRequestHeaders)
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)

import Data.Set qualified as Set
import Db hiding (restaurantId)
import Endpoints qualified
import Fetch (FetchError (..))
import OpenStreetMap (nearest, parseOverpass)
import Places
import Presence (Activity (..), Person (..))
import Properties qualified
import Provider (Provider (..), searchWithFallback)
import Restaurant
import User
import Vibe
import Yelp (parseYelp)

withTestDb :: (Connection -> IO a) -> IO a
withTestDb act = withConnection ":memory:" $ \conn -> initSchema conn >> act conn

-- | Create a session with the given number of users
sessionWith :: Connection -> Int -> IO (SessionId, [UserId])
sessionWith conn n = do
    s <- newSessionId
    us <- traverse (const newUserId) [1 .. n]
    mapM_ (\u -> addUserToDb conn (User u s)) us
    pure (s, us)

-- | Join a session that isn't full
joinOk :: Connection -> SessionId -> Maybe UserId -> IO UserId
joinOk conn s returning = maybe (fail "session full") pure =<< joinSession conn s returning

-- | Ids written as string literals, in the tests only
instance IsString RestaurantId where
    fromString = RestaurantId . T.pack

like, dislike :: Connection -> UserId -> RestaurantId -> IO ()
like conn u r = recordDecision conn (Decision u r True)
dislike conn u r = recordDecision conn (Decision u r False)

-- | A Nearby Search response in the shape documented by Google
searchResponse :: L8.ByteString
searchResponse =
    L8.unlines
        [ "{\"places\": ["
        , "  {\"id\": \"ChIJ1\", \"displayName\": {\"text\": \"Pho Real\", \"languageCode\": \"en\"},"
        , "   \"location\": {\"latitude\": 48.43, \"longitude\": -123.35},"
        , "   \"rating\": 4.5, \"priceLevel\": \"PRICE_LEVEL_MODERATE\","
        , "   \"googleMapsUri\": \"https://maps.google.com/?cid=1\","
        , "   \"primaryTypeDisplayName\": {\"text\": \"Vietnamese Restaurant\", \"languageCode\": \"en\"},"
        , "   \"photos\": [{\"name\": \"places/ChIJ1/photos/AbC-1_x\", \"widthPx\": 800, \"heightPx\": 600,"
        , "     \"authorAttributions\": [{\"displayName\": \"Sam\", \"uri\": \"//maps.google.com/maps/contrib/1\", \"photoUri\": \"//lh3.googleusercontent.com/a\"}]}],"
        , "   \"generativeSummary\": {\"overview\": {\"text\": \"Bright noodle spot known for its broth.\", \"languageCode\": \"en\"}, \"disclosureText\": {\"text\": \"Summarized with Gemini\", \"languageCode\": \"en\"}},"
        , "   \"editorialSummary\": {\"text\": \"Noodles.\", \"languageCode\": \"en\"}},"
        , "  {\"id\": \"ChIJ2\", \"displayName\": {\"text\": \"New Spot\"},"
        , "   \"location\": {\"latitude\": 48.44, \"longitude\": -123.36}}"
        , "]}"
        ]

parseWith :: (Value -> Parser [Restaurant]) -> L8.ByteString -> Either String [Restaurant]
parseWith parser body = eitherDecode body >>= parseEither parser

parse :: L8.ByteString -> Either String [Restaurant]
parse = parseWith parsePlaces

-- | A Text Search response for a neighbourhood, in Google's shape
foundResponse :: Value
foundResponse =
    either error id . eitherDecode . L8.unlines $
        [ "{\"places\": ["
        , "  {\"displayName\": {\"text\": \"Fernwood\"}, \"formattedAddress\": \"Fernwood, Victoria, BC, Canada\","
        , "   \"location\": {\"latitude\": 48.43, \"longitude\": -123.34}},"
        , "  {\"displayName\": {\"text\": \"Fernwood Inn\"}, \"location\": {\"latitude\": 48.432, \"longitude\": -123.341}}"
        , "]}"
        ]

-- | A restaurant with only the given name and position
plain :: Source -> Text -> Loc -> Restaurant
plain src n = Restaurant src (RestaurantId n) n Nothing Nothing Nothing Nothing [] Nothing

provider :: Text -> Either FetchError [Restaurant] -> Provider
provider n result = Provider n (const (pure result))

main :: IO ()
main = hspec $ do
    describe "endpoints" Endpoints.spec
    describe "properties" Properties.spec
    describe "parsePlaces" $ do
        it "reads the fields shown on the card" $
            parse searchResponse
                `shouldBe` Right
                    [ Restaurant
                        Google
                        "ChIJ1"
                        "Pho Real"
                        (Just "https://maps.google.com/?cid=1")
                        (Just 4.5)
                        (Just Moderate)
                        (Just "Vietnamese Restaurant")
                        [Photo (GooglePhoto "places/ChIJ1/photos/AbC-1_x") (Just (Author "Sam" (Just "https://maps.google.com/maps/contrib/1")))]
                        (Just (Summary "Bright noodle spot known for its broth." (Just "Summarized with Gemini")))
                        (Loc 48.43 (-123.35))
                    , Restaurant Google "ChIJ2" "New Spot" Nothing Nothing Nothing Nothing [] Nothing (Loc 48.44 (-123.36))
                    ]
        it "keeps at most five photos" $ do
            let ph i = "{\"name\": \"places/P/photos/" <> L8.pack (show (i :: Int)) <> "\"}"
                body = "{\"places\": [{\"id\": \"P\", \"displayName\": {\"text\": \"Busy\"}, \"location\": {\"latitude\": 1, \"longitude\": 2}, \"photos\": [" <> L8.intercalate "," (map ph [1 .. 10]) <> "]}]}"
            map (length . photos) <$> parse body `shouldBe` Right [5]
        it "uses the editorial summary when there is no AI one" $ do
            let body = "{\"places\": [{\"id\": \"P\", \"displayName\": {\"text\": \"Cafe\"}, \"location\": {\"latitude\": 1, \"longitude\": 2}, \"editorialSummary\": {\"text\": \"Cosy cafe.\"}}]}"
            map summary <$> parse body `shouldBe` Right [Just (Summary "Cosy cafe." Nothing)]
        it "treats a response with no places as an empty list" $
            parse "{}" `shouldBe` Right []

    describe "parseYelp" $ do
        it "reads a business, treating an empty image as no photo" $
            parseWith
                parseYelp
                "{\"businesses\": [{\"id\": \"y1\", \"name\": \"Jam Cafe\", \"url\": \"https://www.yelp.com/biz/jam\", \"image_url\": \"\", \"rating\": 4.5, \"price\": \"$$\", \"categories\": [{\"alias\": \"breakfast\", \"title\": \"Breakfast & Brunch\"}], \"coordinates\": {\"latitude\": 48.42, \"longitude\": -123.36}}]}"
                `shouldBe` Right [Restaurant Yelp "y1" "Jam Cafe" (Just "https://www.yelp.com/biz/jam") (Just 4.5) (Just Moderate) (Just "Breakfast & Brunch") [] Nothing (Loc 48.42 (-123.36))]

    describe "parseOverpass" $ do
        it "reads nodes and ways, skipping places without a name" $
            parseWith
                parseOverpass
                ( L8.unlines
                    [ "{\"elements\": ["
                    , " {\"type\": \"node\", \"id\": 1, \"lat\": 48.43, \"lon\": -123.35, \"tags\": {\"amenity\": \"restaurant\", \"name\": \"Pho Real\", \"cuisine\": \"vietnamese;noodle\"}},"
                    , " {\"type\": \"way\", \"id\": 2, \"center\": {\"lat\": 48.44, \"lon\": -123.36}, \"tags\": {\"name\": \"Fish Store\", \"cuisine\": \"fish_and_chips\"}},"
                    , " {\"type\": \"node\", \"id\": 3, \"lat\": 48.45, \"lon\": -123.37, \"tags\": {\"amenity\": \"restaurant\"}}"
                    , "]}"
                    ]
                )
                `shouldBe` Right
                    [ Restaurant OpenStreetMap "osm:node/1" "Pho Real" (Just "https://www.openstreetmap.org/node/1") Nothing Nothing (Just "Vietnamese") [] Nothing (Loc 48.43 (-123.35))
                    , Restaurant OpenStreetMap "osm:way/2" "Fish Store" (Just "https://www.openstreetmap.org/way/2") Nothing Nothing (Just "Fish and chips") [] Nothing (Loc 48.44 (-123.36))
                    ]
        it "puts the nearest first" $
            map restaurantId (nearest (Loc 0 0) 2 [plain OpenStreetMap "far" (Loc 0.03 0), plain OpenStreetMap "near" (Loc 0.001 0), plain OpenStreetMap "mid" (Loc 0 0.01)])
                `shouldBe` ["near", "mid"]

    describe "searchWithFallback" $ do
        let here = Loc 48.43 (-123.35)
            found = [plain OpenStreetMap "a" here]
            search ps = searchWithFallback (const (pure ())) ps (Search here [] defaultRadius 0)
        it "uses the first source that finds something" $
            search [provider "down" (Left (ParseError "boom")), provider "empty" (Right []), provider "osm" (Right found)]
                `shouldReturn` Just found
        it "says nothing was found when a source answered with nothing" $
            search [provider "down" (Left (ParseError "boom")), provider "empty" (Right [])] `shouldReturn` Just []
        it "fails when every source failed" $
            search [provider "down" (Left (ParseError "boom"))] `shouldReturn` Nothing

    describe "searching a ring" $ do
        let centre = Loc 48.43 (-123.35)
        it "spreads six circles around the middle of the ring, each as wide as it" $ do
            let circles = ringCircles centre 5000 10000
            length circles `shouldBe` 6
            mapM_ (\(c, r) -> (round (metres centre c) :: Int, r) `shouldBe` (7500, 2500)) circles
        it "keeps each place between the radii once" $ do
            let at i m = plain Google i (Loc (48.43 + m / 111320) (-123.35))
            map restaurantId (inRing centre 5000 10000 [at "inside" 1000, at "ring" 7000, at "ring" 7000, at "beyond" 20000])
                `shouldBe` ["ring"]

    describe "spend" $ do
        let t0 = UTCTime (fromGregorian 2026 11 22) 0
            at secs = addUTCTime secs t0
            limit = Limit 3600 5
            run = foldl (\(_, u) (t, key, cost) -> spend limit t key cost u) (True, Map.empty)
        it "allows up to the limit, then refuses" $ do
            fst (run [(at 0, "a", 1), (at 1, "a", 4)]) `shouldBe` True
            fst (run [(at 0, "a", 1), (at 1, "a", 4), (at 2, "a", 1)]) `shouldBe` False
        it "counts each key separately" $
            fst (run [(at 0, "a", 5), (at 1, "b", 5)]) `shouldBe` True
        it "refuses a cost bigger than what's left, keeping the rest" $ do
            fst (run [(at 0, "a", 2), (at 1, "a", 6)]) `shouldBe` False
            fst (run [(at 0, "a", 2), (at 1, "a", 6), (at 2, "a", 3)]) `shouldBe` True
        it "starts again once the window is over" $
            fst (run [(at 0, "a", 5), (at 3601, "a", 5)]) `shouldBe` True
        it "forgets keys whose window is over" $
            Map.keys (snd (run [(at 0, "a", 1), (at 3601, "b", 1)])) `shouldBe` ["b"]

    describe "recently seen places" $ do
        let t0 = UTCTime (fromGregorian 2026 11 22) 0
            at secs = addUTCTime secs t0
            seen = rememberAt 3600 (at 0) ["p1", "p2"] Map.empty
        it "knows places it was told about" $
            (seenAt 3600 (at 10) "p1" seen, seenAt 3600 (at 10) "other" seen) `shouldBe` (True, False)
        it "forgets them after a while" $
            seenAt 3600 (at 3601) "p1" seen `shouldBe` False
        it "drops old ones when remembering new ones" $
            Map.keys (rememberAt 3600 (at 4000) ["p3"] seen) `shouldBe` ["p3"]

    describe "withKey" $ do
        it "hides the key when the request is shown, as in a logged error" $ do
            let req = withKey (PlacesKey "sekret-key") [] (parseRequest_ "https://places.googleapis.com")
            show req `shouldNotContain` "sekret-key"
        it "is needed: without it the key shows up" $
            show (setRequestHeaders [("X-Goog-Api-Key", "sekret-key")] (parseRequest_ "https://places.googleapis.com"))
                `shouldContain` "sekret-key"

    describe "isPhotoName" $ do
        it "accepts a photo resource name" $
            isPhotoName "places/ChIJ1/photos/AbC-1_x" `shouldBe` True
        it "rejects anything else" $
            mapM_
                ((`shouldBe` False) . isPhotoName)
                [ ""
                , "places/ChIJ1"
                , "places/ChIJ1/photos/"
                , "places/ChIJ1/photos/a/media"
                , "places/../photos/a"
                , "places/ChIJ1/photos/a?key=x"
                , "v1/places:searchNearby"
                ]

    describe "Loc JSON" $ do
        it "reads a real place" $
            eitherDecode "{\"latitude\":48.43,\"longitude\":-123.35}" `shouldBe` Right (Loc 48.43 (-123.35))
        it "rejects coordinates off the globe, and null" $
            mapM_
                (\body -> (eitherDecode body :: Either String Loc) `shouldSatisfy` either (const True) (const False))
                [ "{\"latitude\":500,\"longitude\":0}"
                , "{\"latitude\":0,\"longitude\":-180.5}"
                , "{\"latitude\":null,\"longitude\":0}"
                ]

    describe "Price" $ do
        it "is sent as the symbols the frontend shows" $
            map toJSON [minBound .. maxBound :: Price] `shouldBe` ["Free", "$", "$$", "$$$", "$$$$"]
        it "reads Yelp's symbols" $
            map priceFromSymbol ["$", "$$$$", "cheap"] `shouldBe` [Just Inexpensive, Just VeryExpensive, Nothing]

    describe "Restaurant JSON" $ do
        -- The frontend's decoders depend on this exact shape. If a change is
        -- meant, delete the file under .golden and run the tests again.
        it "keeps the shape the frontend reads"
            $ defaultGolden "restaurant"
            $ L8.unpack . encodePretty
            $ [ Restaurant
                    Google
                    "ChIJ1"
                    "Pho Real"
                    (Just "https://maps.google.com/?cid=1")
                    (Just 4.5)
                    (Just Moderate)
                    (Just "Vietnamese Restaurant")
                    [ Photo (GooglePhoto "places/ChIJ1/photos/p1") (Just (Author "Sam" (Just "https://maps.google.com/contrib/1")))
                    , Photo (PhotoUrl "https://example.com/pho.jpg") Nothing
                    ]
                    (Just (Summary "Big bowls of pho." (Just "Summarized with Gemini")))
                    (Loc 48.43 (-123.35))
              , Restaurant OpenStreetMap "osm:node/1" "Fish Store" Nothing Nothing Nothing Nothing [] Nothing (Loc 48.44 (-123.36))
              ]
        it "sends null for missing details" $
            case toJSON (Restaurant Google "ChIJ2" "New Spot" Nothing Nothing Nothing Nothing [] Nothing (Loc 1 2)) of
                Object o -> (KM.lookup "photos" o, KM.lookup "rating" o) `shouldBe` (Just (Array mempty), Just Null)
                v -> expectationFailure ("not an object: " ++ show v)

    describe "isMatch" $ do
        it "is a match once two users in the session like it" $ withTestDb $ \conn -> do
            (s, [u1, u2]) <- sessionWith conn 2
            like conn u1 "a"
            isMatch conn s "a" `shouldReturn` False
            like conn u2 "a"
            isMatch conn s "a" `shouldReturn` True
        it "only counts the restaurant asked about" $ withTestDb $ \conn -> do
            (s, [u1, u2]) <- sessionWith conn 2
            mapM_ (like conn u1) ["a", "b"]
            like conn u2 "a"
            isMatch conn s "b" `shouldReturn` False
        it "ignores the same user liking twice" $ withTestDb $ \conn -> do
            (s, [u1, _]) <- sessionWith conn 2
            like conn u1 "a" >> like conn u1 "a"
            isMatch conn s "a" `shouldReturn` False
        it "does not count dislikes" $ withTestDb $ \conn -> do
            (s, [u1, u2]) <- sessionWith conn 2
            like conn u1 "a" >> dislike conn u2 "a"
            isMatch conn s "a" `shouldReturn` False
        it "uses the latest decision when a user changes their mind" $ withTestDb $ \conn -> do
            (s, [u1, u2]) <- sessionWith conn 2
            like conn u1 "a" >> dislike conn u2 "a" >> like conn u2 "a"
            isMatch conn s "a" `shouldReturn` True
        it "ignores likes from other sessions" $ withTestDb $ \conn -> do
            (s, [u1]) <- sessionWith conn 1
            (_, [other]) <- sessionWith conn 1
            like conn u1 "a" >> like conn other "a"
            isMatch conn s "a" `shouldReturn` False

    describe "joinSession" $ do
        it "gives a new person a new id" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            a <- joinOk conn s Nothing
            b <- joinOk conn s Nothing
            a `shouldNotBe` b
        it "keeps the id of someone coming back to the session" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            a <- joinOk conn s Nothing
            joinSession conn s (Just a) `shouldReturn` Just a
        it "doesn't let an id from another session in" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            (other, _) <- sessionWith conn 0
            a <- joinOk conn other Nothing
            b <- joinOk conn s (Just a)
            b `shouldNotBe` a
        it "treats an unknown id as a new person" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            stranger <- newUserId
            b <- joinOk conn s (Just stranger)
            b `shouldNotBe` stranger
        it "means a reload doesn't turn one person's like into a match" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            me <- joinOk conn s Nothing
            like conn me "a"
            me' <- joinOk conn s (Just me)
            like conn me' "a"
            isMatch conn s "a" `shouldReturn` False
        it "turns away new people once the session is full, but not returning ones" $ withTestDb $ \conn -> do
            (s, u : _) <- sessionWith conn maxPerSession
            joinSession conn s Nothing `shouldReturn` Nothing
            joinSession conn s (Just u) `shouldReturn` Just u

    describe "pruneOlderThan" $ do
        it "deletes old people with their likes and their sessions, keeping recent ones" $ withTestDb $ \conn -> do
            (old, [o]) <- sessionWith conn 1
            (fresh, [f]) <- sessionWith conn 1
            like conn o "a" >> like conn f "a"
            _ <- sessionLocation conn old (Just (Loc 0 0))
            _ <- sessionLocation conn fresh (Just (Loc 0 0))
            _ <- setVibe conn old []
            execute_ conn "UPDATE users SET joined = joined - 100000 WHERE rowid = 1"
            execute_ conn "UPDATE session_vibes SET updated = updated - 100000"
            pruneOlderThan conn 86400
            fmap session <$> getUser conn o `shouldReturn` Nothing
            fmap session <$> getUser conn f `shouldReturn` Just fresh
            decidedBy conn o `shouldReturn` []
            decidedBy conn f `shouldReturn` ["a"]
            query_ conn "SELECT count(*) FROM sessions" `shouldReturn` [Only (1 :: Int)]
            query_ conn "SELECT count(*) FROM session_vibes" `shouldReturn` [Only (0 :: Int)]

    describe "matchesFor" $ do
        it "lists what this user and someone else in the session both liked" $ withTestDb $ \conn -> do
            (s, [me, friend, other]) <- sessionWith conn 3
            (_, [stranger]) <- sessionWith conn 1
            mapM_ (like conn me) ["a", "b", "c", "d"]
            like conn friend "a" >> like conn other "a" >> like conn other "b"
            dislike conn friend "c"
            like conn stranger "d"
            like conn friend "e" >> like conn other "e"
            matchesFor conn (User me s) `shouldReturn` ["a", "b"]
        it "includes a match someone else completed" $ withTestDb $ \conn -> do
            (s, [first, second]) <- sessionWith conn 2
            like conn first "a"
            matchesFor conn (User first s) `shouldReturn` []
            like conn second "a"
            matchesFor conn (User first s) `shouldReturn` ["a"]

    describe "parseFound" $ do
        it "reads each match's name, address and location" $
            parseEither parseFound foundResponse
                `shouldBe` Right
                    [ Found (Place "Fernwood" (Loc 48.43 (-123.34))) "Fernwood, Victoria, BC, Canada"
                    , Found (Place "Fernwood Inn" (Loc 48.432 (-123.341))) ""
                    ]
        it "treats no matches as none" $
            parseEither parseFound (object []) `shouldBe` Right []

    describe "where the group is eating" $ do
        let fernwood = Place "Fernwood" (Loc 48.43 (-123.34))
        it "is set, replaced and cleared" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            getPlace conn s `shouldReturn` Nothing
            setPlace conn s (Just fernwood)
            getPlace conn s `shouldReturn` Just fernwood
            setPlace conn s Nothing
            getPlace conn s `shouldReturn` Nothing
        it "is where the session searches from, over anyone's location" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            setPlace conn s (Just fernwood)
            sessionLocation conn s (Just (Loc 51.04 (-114.1))) `shouldReturn` Just (Loc 48.43 (-123.34))
        it "can't change once someone has started" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            _ <- sessionLocation conn s (Just (Loc 51.04 (-114.1)))
            setPlace conn s (Just fernwood)
            getPlace conn s `shouldReturn` Nothing

    describe "Place JSON"
        $ it "keeps the shape the frontend reads"
        $ defaultGolden "place"
        $ L8.unpack . encodePretty
        $ ( Place "Fernwood" (Loc 48.43 (-123.34))
          , Found (Place "Fernwood" (Loc 48.43 (-123.34))) "Fernwood, Victoria, BC, Canada"
          )

    describe "sessionMembers" $ do
        it "lists everyone in joining order with what they decided on" $ withTestDb $ \conn -> do
            (s, [a, b, c]) <- sessionWith conn 3
            (_, [other]) <- sessionWith conn 1
            like conn a "x" >> dislike conn a "y" >> like conn c "x" >> like conn other "z"
            sessionMembers conn s `shouldReturn` [(a, Set.fromList ["x", "y"]), (b, Set.empty), (c, Set.fromList ["x"])]

    describe "Presence JSON"
        $ it "keeps the shape the frontend reads"
        $ defaultGolden "presence"
        $ L8.unpack . encodePretty
        $ [ Person True Swiping 3 (Just 17)
          , Person False Done 20 (Just 0)
          , Person False Away 0 Nothing
          ]

    describe "othersLiked" $ do
        it "lists what others in the session liked that this user hasn't decided on" $ withTestDb $ \conn -> do
            (s, [me, friend, other]) <- sessionWith conn 3
            (_, [stranger]) <- sessionWith conn 1
            like conn friend "a" >> like conn friend "b" >> dislike conn other "c" >> like conn other "a"
            like conn stranger "d"
            dislike conn me "b"
            othersLiked conn (User me s) `shouldReturn` ["a"]

    describe "Vibe" $ do
        it "turns chips into a text query" $
            queryTerms [Patio, Drinks, Cheap] `shouldBe` "patio cocktails restaurant"
        it "allows the price levels of every price chip picked" $ do
            priceLevels [Cheap] `shouldBe` ["PRICE_LEVEL_INEXPENSIVE", "PRICE_LEVEL_MODERATE"]
            priceLevels [Patio] `shouldBe` []
        it "round trips chip ids" $
            map (chipFromId . chipId) allChips `shouldBe` map Just allChips

    describe "session vibe" $ do
        it "is empty and unlocked for a new session" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            getVibe conn s `shouldReturn` ([], False)
        it "keeps the latest chips anyone set" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            _ <- setVibe conn s [Patio]
            setVibe conn s [Drinks, Cheap, Drinks] `shouldReturn` ([Drinks, Cheap], False)
            getVibe conn s `shouldReturn` ([Drinks, Cheap], False)
        it "locks once someone starts swiping" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            _ <- setVibe conn s [Patio]
            _ <- sessionLocation conn s (Just (Loc 48.43 (-123.35)))
            setVibe conn s [Fancy] `shouldReturn` ([Patio], True)

    describe "searching further" $ do
        it "starts at the default radius" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            _ <- sessionLocation conn s (Just (Loc 48.43 (-123.35)))
            sessionRadius conn s `shouldReturn` defaultRadius
        it "doubles the radius up to the maximum" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            _ <- sessionLocation conn s (Just (Loc 48.43 (-123.35)))
            widenSearch conn s 5000 `shouldReturn` 10000
            widenSearch conn s 10000 `shouldReturn` 20000
            widenSearch conn s 40000 `shouldReturn` maxRadius
            widenSearch conn s maxRadius `shouldReturn` maxRadius
        it "only widens one step when two people ask at once" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            _ <- sessionLocation conn s (Just (Loc 48.43 (-123.35)))
            _ <- widenSearch conn s 5000
            widenSearch conn s 5000 `shouldReturn` 10000
        it "knows what each person has already decided on" $ withTestDb $ \conn -> do
            (_, [me, friend]) <- sessionWith conn 2
            like conn me "a" >> dislike conn me "b" >> like conn friend "c"
            decidedBy conn me `shouldReturn` ["a", "b"]

    describe "sessionLocation" $ do
        it "uses the first location sent for everyone in the session" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            sessionLocation conn s (Just (Loc 48.43 (-123.35))) `shouldReturn` Just (Loc 48.43 (-123.35))
            sessionLocation conn s (Just (Loc 49.28 (-123.12))) `shouldReturn` Just (Loc 48.43 (-123.35))
        it "keeps each session's location separate" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            (other, _) <- sessionWith conn 0
            _ <- sessionLocation conn s (Just (Loc 48.43 (-123.35)))
            sessionLocation conn other (Just (Loc 51.04 (-114.1))) `shouldReturn` Just (Loc 51.04 (-114.1))
        it "gives someone without a location the session's" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            sessionLocation conn s Nothing `shouldReturn` Nothing
            _ <- sessionLocation conn s (Just (Loc 48.43 (-123.35)))
            sessionLocation conn s Nothing `shouldReturn` Just (Loc 48.43 (-123.35))

    describe "initSchema" $ do
        it "upgrades a database from before versioning, keeping likes and dropping stored restaurants" $
            withConnection ":memory:" $ \conn -> do
                execute_ conn "CREATE TABLE restaurants (id TEXT PRIMARY KEY, url TEXT, name Text, rating FLOAT, price TEXT, hours INTEGER, category TEXT, imagelink TEXT, latitude FLOAT, longitude FLOAT)"
                execute_ conn "CREATE TABLE users (userId TEXT, session TEXT)"
                execute_ conn "CREATE TABLE likes (userId TEXT, restaurantId TEXT)"
                execute_ conn "INSERT INTO likes VALUES ('u1', 'a'), ('u1', 'a'), ('u2', 'b')"
                initSchema conn
                query_ conn "SELECT userId, restaurantId, liked FROM decisions ORDER BY userId"
                    `shouldReturn` [("u1", "a", True), ("u2", "b", True) :: (String, String, Bool)]
                query_ conn "SELECT name FROM sqlite_master WHERE type = 'table' ORDER BY name"
                    `shouldReturn` [Only ("decisions" :: String), Only "session_places", Only "session_vibes", Only "sessions", Only "users"]
        it "can run more than once" $ withTestDb $ \conn ->
            initSchema conn `shouldReturn` ()
