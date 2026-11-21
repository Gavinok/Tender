module Main (main) where

import Data.Aeson (Value (Null), eitherDecode, encode, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Value (Object))
import Data.List (unfoldr)
import Database.SQLite.Simple (Connection, execute_, query_, withConnection)
import Test.Hspec

import Db
import Restaurant
import User

restaurant :: String -> Double -> Loc -> Restaurant
restaurant i r l =
    Restaurant
        (Business i "https://example.com" i r Nothing [Hours True] [Category "Thai"])
        (Just "https://example.com/img.jpg")
        l

here :: Loc
here = Loc 48.4 (-123.4)

rid :: Restaurant -> String
rid = bid . bus

ids :: [Restaurant] -> [String]
ids = fmap rid

withTestDb :: (Connection -> IO a) -> IO a
withTestDb act = withConnection ":memory:" $ \conn -> initSchema conn >> act conn

-- | Create a session with the given number of users
sessionWith :: Connection -> Int -> IO (SessionId, [UserId])
sessionWith conn n = do
    s <- newSessionId
    us <- traverse (const newUserId) [1 .. n]
    mapM_ (\u -> addUserToDb conn (User u s)) us
    pure (s, us)

like, dislike :: Connection -> UserId -> String -> IO ()
like conn u r = recordDecision conn (Decision u r True)
dislike conn u r = recordDecision conn (Decision u r False)

main :: IO ()
main = hspec $ do
    describe "nextAfter" $ do
        let rs = [restaurant i 4 here | i <- ["a", "b", "c"]]
        it "starts with the first restaurant" $
            rid <$> nextAfter Nothing rs `shouldBe` Just "a"
        it "returns the restaurant after the given one" $
            rid <$> nextAfter (Just "a") rs `shouldBe` Just "b"
        it "returns nothing after the last restaurant" $
            nextAfter (Just "c") rs `shouldSatisfy` null
        it "visits every restaurant exactly once" $ do
            let step cur = (\r -> (rid r, Just (rid r))) <$> nextAfter cur rs
            unfoldr step Nothing `shouldBe` ids rs

    describe "Restaurant JSON" $ do
        it "sends a null category when there are none" $ do
            let Restaurant b i l = restaurant "a" 4 here
            case toJSON (Restaurant b{categories = []} i l) of
                Object o -> KM.lookup "category" o `shouldBe` Just Null
                v -> expectationFailure ("not an object: " ++ show v)

    describe "Series" $ do
        let business i coords =
                object
                    [ "id" .= (i :: String)
                    , "url" .= ("u" :: String)
                    , "name" .= i
                    , "rating" .= (4 :: Double)
                    , "price" .= Null
                    , "categories" .= ([] :: [Value])
                    , "hours" .= ([] :: [Value])
                    , "coordinates" .= coords
                    ]
            response bs = object ["data" .= object ["search" .= object ["business" .= bs]]]
        it "keeps each business's coordinates and drops those without" $ do
            let body = response [business "a" (object ["latitude" .= (1 :: Double), "longitude" .= (2 :: Double)]), business "b" Null]
            case eitherDecode (encode body) of
                Right (Series [r]) -> (rid r, latitude (location r), longitude (location r)) `shouldBe` ("a", 1, 2)
                other -> expectationFailure (show other)

    describe "matchedRestaurant" $ do
        it "is a match once two users in the session like it" $ withTestDb $ \conn -> do
            storeInDB conn [restaurant "a" 4 here]
            (s, [u1, u2]) <- sessionWith conn 2
            like conn u1 "a"
            (fmap rid <$> matchedRestaurant conn s "a") `shouldReturn` Nothing
            like conn u2 "a"
            (fmap rid <$> matchedRestaurant conn s "a") `shouldReturn` Just "a"
        it "only reports the restaurant that was asked about" $ withTestDb $ \conn -> do
            storeInDB conn [restaurant "a" 4 here, restaurant "b" 4 here]
            (s, [u1, u2]) <- sessionWith conn 2
            mapM_ (like conn u1) ["a", "b"]
            like conn u2 "a"
            (fmap rid <$> matchedRestaurant conn s "b") `shouldReturn` Nothing
        it "ignores the same user liking twice" $ withTestDb $ \conn -> do
            storeInDB conn [restaurant "a" 4 here]
            (s, [u1, _]) <- sessionWith conn 2
            like conn u1 "a" >> like conn u1 "a"
            (fmap rid <$> matchedRestaurant conn s "a") `shouldReturn` Nothing
        it "does not count dislikes" $ withTestDb $ \conn -> do
            storeInDB conn [restaurant "a" 4 here]
            (s, [u1, u2]) <- sessionWith conn 2
            like conn u1 "a" >> dislike conn u2 "a"
            (fmap rid <$> matchedRestaurant conn s "a") `shouldReturn` Nothing
        it "uses the latest decision when a user changes their mind" $ withTestDb $ \conn -> do
            storeInDB conn [restaurant "a" 4 here]
            (s, [u1, u2]) <- sessionWith conn 2
            like conn u1 "a" >> dislike conn u2 "a" >> like conn u2 "a"
            (fmap rid <$> matchedRestaurant conn s "a") `shouldReturn` Just "a"
        it "ignores likes from other sessions" $ withTestDb $ \conn -> do
            storeInDB conn [restaurant "a" 4 here]
            (s, [u1]) <- sessionWith conn 1
            (_, [other]) <- sessionWith conn 1
            like conn u1 "a" >> like conn other "a"
            (fmap rid <$> matchedRestaurant conn s "a") `shouldReturn` Nothing

    describe "initSchema" $ do
        it "keeps likes from a database created before decisions were stored" $
            withConnection ":memory:" $ \conn -> do
                execute_ conn "CREATE TABLE restaurants (id TEXT PRIMARY KEY, url TEXT, name Text, rating FLOAT, price TEXT, hours INTEGER, category TEXT, imagelink TEXT, latitude FLOAT, longitude FLOAT)"
                execute_ conn "CREATE TABLE users (userId TEXT, session TEXT)"
                execute_ conn "CREATE TABLE likes (userId TEXT, restaurantId TEXT)"
                execute_ conn "INSERT INTO likes VALUES ('u1', 'a'), ('u1', 'a'), ('u2', 'b')"
                initSchema conn
                query_ conn "SELECT userId, restaurantId, liked FROM decisions ORDER BY userId"
                    `shouldReturn` [("u1", "a", True), ("u2", "b", True) :: (String, String, Bool)]
        it "can run more than once" $ withTestDb $ \conn ->
            initSchema conn `shouldReturn` ()

    describe "inDB" $ do
        it "returns nearby restaurants best rated first, then by id" $ withTestDb $ \conn -> do
            storeInDB conn [restaurant "c" 4 here, restaurant "b" 4 here, restaurant "a" 3 here, restaurant "z" 5 here]
            ids <$> inDB conn here `shouldReturn` ["z", "b", "c", "a"]
        it "leaves out restaurants far away" $ withTestDb $ \conn -> do
            storeInDB conn [restaurant "near" 4 (Loc 48.45 (-123.35)), restaurant "far" 4 (Loc 49.3 (-123.1))]
            ids <$> inDB conn here `shouldReturn` ["near"]
        it "round trips through the database" $ withTestDb $ \conn -> do
            let r = restaurant "a" 4.5 here
            storeInDB conn [r]
            fmap show <$> inDB conn here `shouldReturn` [show r]
