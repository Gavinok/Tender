module Main (main) where

import Data.Aeson (Value (Null), eitherDecode, toJSON)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Value (Object), parseEither)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import Database.SQLite.Simple (Connection, Only (..), execute_, query_, withConnection)
import Test.Hspec

import Db
import Places
import Restaurant
import User

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

parse :: L8.ByteString -> Either String [Restaurant]
parse body = eitherDecode body >>= parseEither parsePlaces

main :: IO ()
main = hspec $ do
    describe "parsePlaces" $ do
        it "reads the fields shown on the card" $
            parse searchResponse
                `shouldBe` Right
                    [ Restaurant
                        "ChIJ1"
                        "Pho Real"
                        (Just "https://maps.google.com/?cid=1")
                        (Just 4.5)
                        (Just "$$")
                        (Just "Vietnamese Restaurant")
                        (Just (Photo "places/ChIJ1/photos/AbC-1_x" (Just (Author "Sam" (Just "https://maps.google.com/maps/contrib/1")))))
                        (Loc 48.43 (-123.35))
                    , Restaurant "ChIJ2" "New Spot" Nothing Nothing Nothing Nothing Nothing (Loc 48.44 (-123.36))
                    ]
        it "treats a response with no places as an empty list" $
            parse "{}" `shouldBe` Right []

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
            case toJSON (Restaurant "ChIJ2" "New Spot" Nothing Nothing Nothing Nothing Nothing (Loc 1 2)) of
                Object o -> (KM.lookup "photo" o, KM.lookup "rating" o) `shouldBe` (Just Null, Just Null)
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
                    `shouldReturn` [Only ("decisions" :: String), Only "users"]
        it "can run more than once" $ withTestDb $ \conn ->
            initSchema conn `shouldReturn` ()
