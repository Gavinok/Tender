{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Db (initDb, withDb, setImage, storeInDB, inDB, addUserToDb, addLikeToDb, matchedRestaurant, Like (..), getUser) where

import Database.SQLite.Simple (
    Connection,
    Only (..),
    ToRow,
    execute,
    executeMany,
    execute_,
    query,
    withConnection,
 )
import Data.Maybe (listToMaybe)
import GHC.Generics (Generic)
import Restaurant
import qualified User as U

{- | Open a connection for the duration of the action, closing it even if
the action throws.
-}
withDb :: FilePath -> (Connection -> IO a) -> IO a
withDb path act = withConnection path $ \conn -> do
    -- Wait for concurrent writers instead of failing with SQLITE_BUSY
    execute_ conn "PRAGMA busy_timeout = 5000"
    act conn

-- | Create the schema. Run once at startup.
initDb :: FilePath -> IO ()
initDb path = withDb path $ \conn -> do
    let ex = execute_ conn
    ex "PRAGMA journal_mode = WAL"
    -- Restaurants
    ex $
        mconcat
            [ "CREATE TABLE IF NOT EXISTS restaurants "
            , "(id TEXT PRIMARY KEY, url TEXT, name Text, rating FLOAT, price TEXT, hours INTEGER, category TEXT,"
            , " imagelink TEXT,"
            , " latitude FLOAT, longitude FLOAT"
            , " )"
            ]
    -- Users
    ex $
        mconcat
            [ "CREATE TABLE IF NOT EXISTS users "
            , "(userId TEXT, session TEXT)"
            ]
    -- Likes
    ex $
        mconcat
            [ "CREATE TABLE IF NOT EXISTS likes "
            , "(userId TEXT, restaurantId TEXT)"
            ]

-- | Set the image url of the restaurant with the given id
setImage :: Connection -> String -> String -> IO ()
setImage conn restaurant img =
    execute conn "UPDATE restaurants SET imagelink = ? WHERE id = ?" (img, restaurant)

addUserToDb :: Connection -> U.User -> IO ()
addUserToDb conn (U.User i s) =
    execute conn "INSERT INTO users (userId, session) VALUES (?, ?)" (i, s)

data Like = Like
    { userId :: !U.UserId
    , restaurantId :: !String
    }
    deriving (Show, Generic)
    deriving anyclass (ToRow)

addLikeToDb :: Connection -> Like -> IO ()
addLikeToDb conn (Like u r) =
    execute conn "INSERT INTO likes (userId, restaurantId) VALUES (?, ?)" (u, r)

getUser :: Connection -> U.UserId -> IO (Maybe U.User)
getUser conn u =
    listToMaybe <$> query conn "SELECT userId, session FROM users WHERE userId = ?" (Only u)

{- | The given restaurant if more than one user in the session has liked
it
-}
matchedRestaurant :: Connection -> U.SessionId -> String -> IO (Maybe Restaurant)
matchedRestaurant conn s restaurant =
    listToMaybe
        <$> query
        conn
        ( mconcat
            [ "WITH l AS (SELECT DISTINCT likes.userId, likes.restaurantId FROM likes) "
            , "SELECT restaurants.id, restaurants.url, restaurants.name, restaurants.rating, restaurants.price, restaurants.hours, restaurants.category, restaurants.imagelink, restaurants.latitude, restaurants.longitude "
            , "FROM users JOIN l "
            , "ON users.userId IS l.userId "
            , "JOIN restaurants "
            , "ON l.restaurantId is restaurants.id "
            , "WHERE users.session = ? AND l.restaurantId = ? "
            , "GROUP BY l.restaurantId "
            , "HAVING count(l.restaurantId) > 1 "
            ]
        )
        (s, restaurant)

storeInDB :: Connection -> [Restaurant] -> IO ()
storeInDB conn =
    executeMany conn "INSERT OR IGNORE INTO restaurants (id, url, name, rating, price, hours, category, imagelink, latitude, longitude) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

{- | How far from the user, in degrees, to look for stored restaurants.
0.15 degrees of latitude is roughly 16km.
-}
searchRange :: Double
searchRange = 0.15

-- | Restaurants already stored near the given location
inDB :: Connection -> Loc -> IO [Restaurant]
inDB conn l = do
    let range = searchRange
        baseQuery = "SELECT * FROM restaurants WHERE (latitude BETWEEN ? AND ?) AND (longitude BETWEEN ? AND ?)"
    query conn baseQuery (latitude l - range, latitude l + range, longitude l - range, longitude l + range)
