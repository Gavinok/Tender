{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Db (initDb, initSchema, withDb, addUserToDb, joinSession, sessionLocation, recordDecision, isMatch, othersLiked, Decision (..), getUser) where

import Control.Monad (forM_)
import Database.SQLite.Simple (
    Connection,
    Only (..),
    Query,
    ToRow,
    execute,
    execute_,
    query,
    query_,
    withConnection,
    withTransaction,
 )
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import GHC.Generics (Generic)
import Data.Text (Text)
import Restaurant (Loc (..))
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
    , -- 3: restaurants come fresh from Google Places, whose terms don't allow
      -- storing them. Decisions keep only the place id, which is allowed.
      ["DROP TABLE restaurants"]
    , -- 4: one location per session, so everyone in it sees the same restaurants
      ["CREATE TABLE sessions (session TEXT PRIMARY KEY, latitude REAL NOT NULL, longitude REAL NOT NULL)"]
    ]

addUserToDb :: Connection -> U.User -> IO ()
addUserToDb conn (U.User i s) =
    execute conn "INSERT INTO users (userId, session) VALUES (?, ?)" (i, s)

{- | The user's id in the session. Someone coming back (after a reload, say)
keeps the id they had, so their earlier likes still count as theirs and
not as another person's. Anyone else, including an id from another
session, joins as a new user.
-}
joinSession :: Connection -> U.SessionId -> Maybe U.UserId -> IO U.UserId
joinSession conn s returning = do
    known <- maybe (pure Nothing) (getUser conn) returning
    case known of
        Just u | U.session u == s -> pure (U.userId u)
        _ -> do
            uid <- U.newUserId
            addUserToDb conn (U.User uid s)
            pure uid

{- | Where the session searches for restaurants: the first location anyone
in it sent. Everyone else gets that one too, so they all see the same
restaurants and can match on them.
-}
sessionLocation :: Connection -> U.SessionId -> Loc -> IO Loc
sessionLocation conn s (Loc lat lon) = do
    execute conn "INSERT OR IGNORE INTO sessions (session, latitude, longitude) VALUES (?, ?, ?)" (s, lat, lon)
    found <- query conn "SELECT latitude, longitude FROM sessions WHERE session = ?" (Only s)
    pure $ case found of
        [(lat', lon')] -> Loc lat' lon'
        _ -> Loc lat lon

-- | Whether a user liked a restaurant
data Decision = Decision
    { userId :: !U.UserId
    , restaurantId :: !Text
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

{- | Restaurants someone else in the session has liked that this user hasn't
decided on yet, so they can be shown first
-}
othersLiked :: Connection -> U.User -> IO [Text]
othersLiked conn u =
    map fromOnly
        <$> query
            conn
            ( mconcat
                [ "SELECT DISTINCT decisions.restaurantId FROM decisions "
                , "JOIN users ON users.userId = decisions.userId "
                , "WHERE users.session = ? AND decisions.userId != ? AND decisions.liked "
                , "AND decisions.restaurantId NOT IN (SELECT restaurantId FROM decisions WHERE userId = ?)"
                ]
            )
            (U.session u, U.userId u, U.userId u)

-- | Whether more than one person in the session has liked the restaurant
isMatch :: Connection -> U.SessionId -> Text -> IO Bool
isMatch conn s restaurant = do
    likes <-
        query
            conn
            ( mconcat
                [ "SELECT count(*) FROM decisions JOIN users ON users.userId = decisions.userId "
                , "WHERE users.session = ? AND decisions.restaurantId = ? AND decisions.liked"
                ]
            )
            (s, restaurant)
    pure $ case likes of
        [Only n] -> n > (1 :: Int)
        _ -> False
