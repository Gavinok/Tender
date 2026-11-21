{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Db (initDb, initSchema, withDb, setImage, storeInDB, inDB, addUserToDb, recordDecision, matchedRestaurant, Decision (..), getUser) where

import Control.Monad (forM_)
import Database.SQLite.Simple (
    Connection,
    Only (..),
    Query,
    ToRow,
    execute,
    executeMany,
    execute_,
    query,
    query_,
    withConnection,
    withTransaction,
 )
import Data.Maybe (listToMaybe)
import Data.String (fromString)
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
    execute_ conn "PRAGMA journal_mode = WAL"
    initSchema conn

{- | Bring the schema up to date. The database's user_version records how
many of the migrations have been applied.
-}
initSchema :: Connection -> IO ()
initSchema conn = do
    applied <- maybe 0 fromOnly . listToMaybe <$> query_ conn "PRAGMA user_version"
    forM_ (drop applied (zip [1 :: Int ..] migrations)) $ \(version, steps) ->
        withTransaction conn $ do
            mapM_ (execute_ conn) steps
            -- PRAGMA does not accept parameters
            execute_ conn (fromString ("PRAGMA user_version = " ++ show version))

-- | Each entry is applied once, in order. Only ever append to this list.
migrations :: [[Query]]
migrations =
    [ -- 1: the original schema, already present in databases from before versioning
      [ mconcat
            [ "CREATE TABLE IF NOT EXISTS restaurants "
            , "(id TEXT PRIMARY KEY, url TEXT, name Text, rating FLOAT, price TEXT, hours INTEGER, category TEXT,"
            , " imagelink TEXT,"
            , " latitude FLOAT, longitude FLOAT"
            , " )"
            ]
      , "CREATE TABLE IF NOT EXISTS users (userId TEXT, session TEXT)"
      , "CREATE TABLE IF NOT EXISTS likes (userId TEXT, restaurantId TEXT)"
      ]
    , -- 2: record dislikes as well as likes, one decision per user and restaurant
      [ mconcat
            [ "CREATE TABLE decisions "
            , "(userId TEXT NOT NULL, restaurantId TEXT NOT NULL, liked INTEGER NOT NULL,"
            , " PRIMARY KEY (userId, restaurantId))"
            ]
      , "INSERT OR IGNORE INTO decisions SELECT userId, restaurantId, 1 FROM likes"
      , "DROP TABLE likes"
      ]
    ]

-- | Set the image url of the restaurant with the given id
setImage :: Connection -> String -> String -> IO ()
setImage conn restaurant img =
    execute conn "UPDATE restaurants SET imagelink = ? WHERE id = ?" (img, restaurant)

addUserToDb :: Connection -> U.User -> IO ()
addUserToDb conn (U.User i s) =
    execute conn "INSERT INTO users (userId, session) VALUES (?, ?)" (i, s)

-- | Whether a user liked a restaurant
data Decision = Decision
    { userId :: !U.UserId
    , restaurantId :: !String
    , liked :: !Bool
    }
    deriving (Show, Generic)
    deriving anyclass (ToRow)

-- | Record a decision, replacing any earlier one for the same restaurant
recordDecision :: Connection -> Decision -> IO ()
recordDecision conn =
    execute
        conn
        ( mconcat
            [ "INSERT INTO decisions (userId, restaurantId, liked) VALUES (?, ?, ?) "
            , "ON CONFLICT (userId, restaurantId) DO UPDATE SET liked = excluded.liked"
            ]
        )

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
            [ "WITH l AS (SELECT userId, restaurantId FROM decisions WHERE liked) "
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
        -- Order must be stable since the frontend steps through by id
        baseQuery = "SELECT * FROM restaurants WHERE (latitude BETWEEN ? AND ?) AND (longitude BETWEEN ? AND ?) ORDER BY rating DESC, id"
    query conn baseQuery (latitude l - range, latitude l + range, longitude l - range, longitude l + range)
