module Main (main) where

import Data.Aeson (Value (Array, Null), eitherDecode, toJSON)
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Value (Object), parseEither)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import Database.SQLite.Simple (Connection, Only (..), execute_, query_, withConnection)
import Test.Hspec

import Db hiding (restaurantId)
import Fetch (FetchError (..))
import OpenStreetMap (nearest, parseOverpass)
import Places
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

like, dislike :: Connection -> UserId -> Text -> IO ()
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
        , "     \"authorAttributions\": [{\"displayName\": \"Sam\", \"uri\": \"//maps.google.com/maps/contrib/1\", \"photoUri\": \"//lh3.googleusercontent.com/a\"}]}]},"
        , "  {\"id\": \"ChIJ2\", \"displayName\": {\"text\": \"New Spot\"},"
        , "   \"location\": {\"latitude\": 48.44, \"longitude\": -123.36}}"
        , "]}"
        ]

parseWith :: (Value -> Parser [Restaurant]) -> L8.ByteString -> Either String [Restaurant]
parseWith parser body = eitherDecode body >>= parseEither parser

parse :: L8.ByteString -> Either String [Restaurant]
parse = parseWith parsePlaces

-- | A restaurant with only the given name and position
plain :: Source -> Text -> Loc -> Restaurant
plain src n = Restaurant src n n Nothing Nothing Nothing Nothing []

provider :: Text -> Either FetchError [Restaurant] -> Provider
provider n result = Provider n (const (pure result))

main :: IO ()
main = hspec $ do
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
                        (Just "$$")
                        (Just "Vietnamese Restaurant")
                        [Photo (GooglePhoto "places/ChIJ1/photos/AbC-1_x") (Just (Author "Sam" (Just "https://maps.google.com/maps/contrib/1")))]
                        (Loc 48.43 (-123.35))
                    , Restaurant Google "ChIJ2" "New Spot" Nothing Nothing Nothing Nothing [] (Loc 48.44 (-123.36))
                    ]
        it "keeps at most five photos" $ do
            let ph i = "{\"name\": \"places/P/photos/" <> L8.pack (show (i :: Int)) <> "\"}"
                body = "{\"places\": [{\"id\": \"P\", \"displayName\": {\"text\": \"Busy\"}, \"location\": {\"latitude\": 1, \"longitude\": 2}, \"photos\": [" <> L8.intercalate "," (map ph [1 .. 10]) <> "]}]}"
            map (length . photos) <$> parse body `shouldBe` Right [5]
        it "treats a response with no places as an empty list" $
            parse "{}" `shouldBe` Right []

    describe "parseYelp" $ do
        it "reads a business, treating an empty image as no photo" $
            parseWith parseYelp
                "{\"businesses\": [{\"id\": \"y1\", \"name\": \"Jam Cafe\", \"url\": \"https://www.yelp.com/biz/jam\", \"image_url\": \"\", \"rating\": 4.5, \"price\": \"$$\", \"categories\": [{\"alias\": \"breakfast\", \"title\": \"Breakfast & Brunch\"}], \"coordinates\": {\"latitude\": 48.42, \"longitude\": -123.36}}]}"
                `shouldBe` Right [Restaurant Yelp "y1" "Jam Cafe" (Just "https://www.yelp.com/biz/jam") (Just 4.5) (Just "$$") (Just "Breakfast & Brunch") [] (Loc 48.42 (-123.36))]

    describe "parseOverpass" $ do
        it "reads nodes and ways, skipping places without a name" $
            parseWith parseOverpass
                ( L8.unlines
                    [ "{\"elements\": ["
                    , " {\"type\": \"node\", \"id\": 1, \"lat\": 48.43, \"lon\": -123.35, \"tags\": {\"amenity\": \"restaurant\", \"name\": \"Pho Real\", \"cuisine\": \"vietnamese;noodle\"}},"
                    , " {\"type\": \"way\", \"id\": 2, \"center\": {\"lat\": 48.44, \"lon\": -123.36}, \"tags\": {\"name\": \"Fish Store\", \"cuisine\": \"fish_and_chips\"}},"
                    , " {\"type\": \"node\", \"id\": 3, \"lat\": 48.45, \"lon\": -123.37, \"tags\": {\"amenity\": \"restaurant\"}}"
                    , "]}"
                    ]
                )
                `shouldBe` Right
                    [ Restaurant OpenStreetMap "osm:node/1" "Pho Real" (Just "https://www.openstreetmap.org/node/1") Nothing Nothing (Just "Vietnamese") [] (Loc 48.43 (-123.35))
                    , Restaurant OpenStreetMap "osm:way/2" "Fish Store" (Just "https://www.openstreetmap.org/way/2") Nothing Nothing (Just "Fish and chips") [] (Loc 48.44 (-123.36))
                    ]
        it "puts the nearest first" $
            map restaurantId (nearest (Loc 0 0) 2 [plain OpenStreetMap "far" (Loc 0.03 0), plain OpenStreetMap "near" (Loc 0.001 0), plain OpenStreetMap "mid" (Loc 0 0.01)])
                `shouldBe` ["near", "mid"]

    describe "searchWithFallback" $ do
        let here = Loc 48.43 (-123.35)
            found = [plain OpenStreetMap "a" here]
            search ps = searchWithFallback (const (pure ())) ps (Search here [])
        it "uses the first source that finds something" $
            search [provider "down" (Left (ParseError "boom")), provider "empty" (Right []), provider "osm" (Right found)]
                `shouldReturn` Just found
        it "says nothing was found when a source answered with nothing" $
            search [provider "down" (Left (ParseError "boom")), provider "empty" (Right [])] `shouldReturn` Just []
        it "fails when every source failed" $
            search [provider "down" (Left (ParseError "boom"))] `shouldReturn` Nothing

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

    describe "Restaurant JSON" $ do
        it "sends null for missing details" $
            case toJSON (Restaurant Google "ChIJ2" "New Spot" Nothing Nothing Nothing Nothing [] (Loc 1 2)) of
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
            a <- joinSession conn s Nothing
            b <- joinSession conn s Nothing
            a `shouldNotBe` b
        it "keeps the id of someone coming back to the session" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            a <- joinSession conn s Nothing
            joinSession conn s (Just a) `shouldReturn` a
        it "doesn't let an id from another session in" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            (other, _) <- sessionWith conn 0
            a <- joinSession conn other Nothing
            b <- joinSession conn s (Just a)
            b `shouldNotBe` a
        it "treats an unknown id as a new person" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            stranger <- newUserId
            b <- joinSession conn s (Just stranger)
            b `shouldNotBe` stranger
        it "means a reload doesn't turn one person's like into a match" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            me <- joinSession conn s Nothing
            like conn me "a"
            me' <- joinSession conn s (Just me)
            like conn me' "a"
            isMatch conn s "a" `shouldReturn` False

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
            _ <- sessionLocation conn s (Loc 48.43 (-123.35))
            setVibe conn s [Fancy] `shouldReturn` ([Patio], True)

    describe "sessionLocation" $ do
        it "uses the first location sent for everyone in the session" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            sessionLocation conn s (Loc 48.43 (-123.35)) `shouldReturn` Loc 48.43 (-123.35)
            sessionLocation conn s (Loc 49.28 (-123.12)) `shouldReturn` Loc 48.43 (-123.35)
        it "keeps each session's location separate" $ withTestDb $ \conn -> do
            (s, _) <- sessionWith conn 0
            (other, _) <- sessionWith conn 0
            _ <- sessionLocation conn s (Loc 48.43 (-123.35))
            sessionLocation conn other (Loc 51.04 (-114.1)) `shouldReturn` Loc 51.04 (-114.1)

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
                    `shouldReturn` [Only ("decisions" :: String), Only "session_vibes", Only "sessions", Only "users"]
        it "can run more than once" $ withTestDb $ \conn ->
            initSchema conn `shouldReturn` ()
