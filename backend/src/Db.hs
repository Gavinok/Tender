{-# LANGUAGE DeriveAnyClass #-}

module Db (initDb, initSchema, DbPool, openPool, withConn, addUserToDb, joinSession, maxPerSession, pruneOlderThan, sessionLocation, sessionRadius, widenSearch, decidedBy, getVibe, setVibe, recordDecision, isMatch, matchesFor, othersLiked, Decision (..), getUser) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Pool (Pool, defaultPoolConfig, newPool, setNumStripes, withResource)
import Database.SQLite.Simple (
    Connection,
    Only (..),
    Query,
    ToRow,
    execute,
    execute_,
    query,
    query_,
    close,
    open,
    withTransaction,
 )
import Data.List (nub)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import Vibe (Chip, chipFromId, chipId, defaultRadius, maxRadius)
import Data.String (fromString)
import GHC.Generics (Generic)
import Restaurant (Loc (..), RestaurantId)
import qualified User as U

openDb :: FilePath -> IO Connection
openDb path = do
    conn <- open path
    -- Wait for concurrent writers instead of failing with SQLITE_BUSY
    execute_ conn "PRAGMA busy_timeout = 5000"
    pure conn

-- | Open connections, kept for reuse between requests
type DbPool = Pool Connection

{- | A few connections, closed after a minute unused. The app runs as one
instance, so one pool is all of them.
-}
openPool :: FilePath -> IO DbPool
openPool path = newPool (setNumStripes (Just 1) (defaultPoolConfig (openDb path) close 60 4))

-- | Run the action with a connection from the pool, returning it after
withConn :: DbPool -> (Connection -> IO a) -> IO a
withConn = withResource

-- | Create the schema. Run once at startup.
initDb :: FilePath -> IO ()
initDb path = bracket (openDb path) close $ \conn -> do
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
    , -- 5: vibe chips the group picks in the lobby, as comma separated ids
      ["CREATE TABLE session_vibes (session TEXT PRIMARY KEY, chips TEXT NOT NULL)"]
    , -- 6: how far the session searches, in metres, widened by "search further"
      ["ALTER TABLE sessions ADD COLUMN radius REAL NOT NULL DEFAULT 5000"]
    , -- 7: when people joined and vibes changed, so old ones can be deleted
      [ "ALTER TABLE users ADD COLUMN joined INTEGER NOT NULL DEFAULT 0"
      , "ALTER TABLE session_vibes ADD COLUMN updated INTEGER NOT NULL DEFAULT 0"
      ]
    ]

addUserToDb :: Connection -> U.User -> IO ()
addUserToDb conn (U.User i s) =
    execute conn "INSERT INTO users (userId, session, joined) VALUES (?, ?, strftime('%s', 'now'))" (i, s)

-- | Most people in one session
maxPerSession :: Int
maxPerSession = 30

{- | The user's id in the session. Someone coming back (after a reload, say)
keeps the id they had, so their earlier likes still count as theirs and
not as another person's. Anyone else, including an id from another
session, joins as a new user.
-}
joinSession :: Connection -> U.SessionId -> Maybe U.UserId -> IO (Maybe U.UserId)
joinSession conn s returning = do
    known <- maybe (pure Nothing) (getUser conn) returning
    case known of
        Just u | U.session u == s -> pure (Just (U.userId u))
        _ -> do
            members <- query conn "SELECT count(*) FROM users WHERE session = ?" (Only s)
            if maybe 0 fromOnly (listToMaybe members) >= maxPerSession
                then pure Nothing
                else do
                    uid <- U.newUserId
                    addUserToDb conn (U.User uid s)
                    pure (Just uid)

{- | Delete people who joined more than the given number of seconds ago,
with their likes, and sessions and vibes nobody is in anymore. Sessions
only last as long as a meal decision, so a day is plenty.
-}
pruneOlderThan :: Connection -> Int -> IO ()
pruneOlderThan conn secs = withTransaction conn $ do
    let cutoff = "strftime('%s', 'now') - ?"
    execute conn (fromString ("DELETE FROM decisions WHERE userId IN (SELECT userId FROM users WHERE joined < " ++ cutoff ++ ")")) (Only secs)
    execute conn (fromString ("DELETE FROM users WHERE joined < " ++ cutoff)) (Only secs)
    execute_ conn "DELETE FROM sessions WHERE session NOT IN (SELECT session FROM users)"
    execute conn (fromString ("DELETE FROM session_vibes WHERE updated < " ++ cutoff ++ " AND session NOT IN (SELECT session FROM users)")) (Only secs)

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

-- | How far the session searches, in metres
sessionRadius :: Connection -> U.SessionId -> IO Double
sessionRadius conn s = do
    found <- query conn "SELECT radius FROM sessions WHERE session = ?" (Only s)
    pure $ case found of
        [Only r] -> r
        _ -> defaultRadius

{- | Double the session's radius from the one the person was shown, up to
the maximum. Two people pressing "search further" at once only widen it
one step, since both double the same radius.
-}
widenSearch :: Connection -> U.SessionId -> Double -> IO Double
widenSearch conn s from = do
    execute conn "UPDATE sessions SET radius = max(radius, ?) WHERE session = ?" (min maxRadius (from * 2), s)
    sessionRadius conn s

-- | Restaurants the user has already liked or not, to leave out of new searches
decidedBy :: Connection -> U.UserId -> IO [RestaurantId]
decidedBy conn u = map fromOnly <$> query conn "SELECT restaurantId FROM decisions WHERE userId = ? ORDER BY restaurantId" (Only u)

{- | The session's vibe chips, and whether they are locked. They lock once
someone starts swiping, since that runs the session's one search.
-}
getVibe :: Connection -> U.SessionId -> IO ([Chip], Bool)
getVibe conn s = do
    stored <- query conn "SELECT chips FROM session_vibes WHERE session = ?" (Only s)
    started <- query conn "SELECT count(*) FROM sessions WHERE session = ?" (Only s)
    let chips = case stored of
            [Only t] -> mapMaybe chipFromId (T.splitOn "," t)
            _ -> []
    pure (chips, started /= [Only (0 :: Int)])

-- | Replace the session's chips unless they are locked, returning the result
setVibe :: Connection -> U.SessionId -> [Chip] -> IO ([Chip], Bool)
setVibe conn s chips = withTransaction conn $ do
    (_, locked) <- getVibe conn s
    if locked
        then getVibe conn s
        else do
            execute
                conn
                "INSERT INTO session_vibes (session, chips, updated) VALUES (?, ?, strftime('%s', 'now')) ON CONFLICT (session) DO UPDATE SET chips = excluded.chips, updated = excluded.updated"
                (s, T.intercalate "," (map chipId (nub chips)))
            pure (nub chips, False)

-- | Whether a user liked a restaurant
data Decision = Decision
    { userId :: !U.UserId
    , restaurantId :: !RestaurantId
    , liked :: !Bool
    }
    deriving stock (Show, Generic)
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
othersLiked :: Connection -> U.User -> IO [RestaurantId]
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

{- | Restaurants the user liked that someone else in their session liked
too. Sent with every decision, so people hear about matches others made
on their next swipe.
-}
matchesFor :: Connection -> U.User -> IO [RestaurantId]
matchesFor conn u =
    map fromOnly
        <$> query
            conn
            ( mconcat
                [ "SELECT decisions.restaurantId FROM decisions "
                , "JOIN users ON users.userId = decisions.userId "
                , "WHERE users.session = ? AND decisions.liked AND decisions.userId != ? "
                , "AND decisions.restaurantId IN (SELECT restaurantId FROM decisions WHERE userId = ? AND liked) "
                , "GROUP BY decisions.restaurantId ORDER BY decisions.restaurantId"
                ]
            )
            (U.session u, U.userId u, U.userId u)

-- | Whether more than one person in the session has liked the restaurant
isMatch :: Connection -> U.SessionId -> RestaurantId -> IO Bool
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
