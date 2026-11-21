{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Db (addImgToDb, storeInDB, inDB, setupDb, addUserToDb, addLikeToDb, likedRestaurants, LikedRestaurant(..), Like (..), dbClear, getUser) where

import Database.SQLite.Simple (
    Connection (..),
    FromRow,
    ToRow,
    close,
    execute,
    execute_,
    open,
    query,
    query_,
    toRow,
 )
import GHC.Generics (Generic)
import Restaurant
import qualified User as U

data Open
data Closed

newtype DbConnection a = DbConnection {connection :: Connection}

dbConnect :: IO (DbConnection Open)
dbConnect = do
    conn <- open "test.db"
    pure (DbConnection conn)

dbClose :: DbConnection Open -> IO (DbConnection Closed)
dbClose (DbConnection conn) = do
    close conn
    pure $ DbConnection conn

newtype ClosedDbConnection = ClosedDbConnection () deriving (Show)

setupDb :: IO (DbConnection Open)
setupDb = do
    conn <- dbConnect
    let ex = execute_ $ connection conn
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
    pure conn

addImgToDb :: Restaurant -> IO (DbConnection Closed)
addImgToDb (Restaurant b (Just i) _) = do
    putStrLn $ "adding image for " ++ show (bid b)
    conn <- setupDb
    execute (connection conn) "UPDATE restaurants SET imagelink = ? WHERE id = ?" (Just i, bid b)
    dbClose conn
addImgToDb _ = fail "invalid input to update"

addUserToDb :: U.User -> IO (DbConnection Closed)
addUserToDb (U.User i s) = do
    conn <- setupDb
    execute (connection conn) "INSERT INTO users (userId, session) VALUES (?, ?)" (i, s)
    putStrLn $ "adding User " ++ show i ++ " with session " ++ show s
    dbClose conn

addUserToSession :: U.User -> IO (DbConnection Closed)
addUserToSession (U.User i s) = do
    conn <- setupDb
    execute (connection conn) "INSERT INTO users (userId, session) VALUES (?, ?)" (i, s)
    putStrLn $ "adding User " ++ show i ++ " with session " ++ show s
    dbClose conn
data Like = Like
    { userId :: !String
    , restaurantId :: !String
    }
    deriving (Show, Generic)
    deriving anyclass (ToRow)
deriving anyclass instance FromRow Like

addLikeToDb :: Like -> IO (DbConnection Closed)
addLikeToDb (Like u r) = do
    putStrLn $ "adding Like for User " ++ show u ++ " Restaurant " ++ show r
    conn <- setupDb
    execute (connection conn) "INSERT INTO likes (userId, restaurantId) VALUES (?, ?)" (u, r)
    dbClose conn

newtype LikedRestaurant = LikedRestaurant {toString :: String} deriving (Generic, Show)
deriving anyclass instance FromRow LikedRestaurant

getUser :: String -> IO (DbConnection Closed, [Maybe U.User])
getUser u = do
    conn <- setupDb
    r <-
        query
            (connection conn)
            ( mconcat
                [ "SELECT * "
                , "FROM users "
                , "WHERE userId = ?"
                ]
            )
            [u] ::
            IO [Maybe U.User]
    c <- dbClose conn
    pure (c, r)

getFriends :: U.Session -> IO (DbConnection Closed, [Maybe U.User])
getFriends s = do
    conn <- setupDb
    r <-
        query
            (connection conn)
            ( mconcat
                [ "SELECT userId, session "
                , "FROM users "
                , "WHERE users.session = ?"
                ]
            )
            [s]
    c <- dbClose conn
    pure (c, r)

likedRestaurants :: U.Session -> IO (DbConnection Closed, [Restaurant])
likedRestaurants s = do
    conn <- setupDb
    r <-
        query
            (connection conn)
            ( mconcat
                [
                 "WITH l AS (SELECT DISTINCT likes.userId, likes.restaurantId FROM likes) "
                , "SELECT restaurants.id, restaurants.url, restaurants.name, restaurants.rating, restaurants.price, restaurants.hours, restaurants.category, restaurants.imagelink, restaurants.latitude, restaurants.longitude "
                , "FROM users JOIN l "
                , "ON users.userId IS l.userId "
                , "JOIN restaurants "
                , "ON l.restaurantId is restaurants.id "
                , "WHERE users.session = ? "
                , "GROUP BY l.restaurantId "
                , "HAVING count(l.restaurantId) > 1 "
                ]
            )
            [s]
    c <- dbClose conn
    pure (c, r)

storeInDB :: [Restaurant] -> IO (DbConnection Closed)
storeInDB b = do
    conn <- setupDb
    mapM_
        (execute (connection conn) "INSERT OR IGNORE INTO restaurants (id, url, name, rating, price, hours, category, imagelink, latitude, longitude) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
        b
    dbClose conn

-- TODO Use freemonads to mark this DB connection as needing to be closed
inDB :: Loc ->  IO (Either String [Restaurant])
inDB l = do
    conn <- setupDb
    let range = 2
        baseQuery = "SELECT * FROM restaurants WHERE (latitude BETWEEN ? AND ?) AND (longitude BETWEEN ? AND ?)"
    r <- query (connection conn) baseQuery (latitude l - range, latitude l + range, longitude l - range, longitude l + range) :: IO [Restaurant]
    dbClose conn
    case r of
        [] -> do
            putStrLn $ "at " ++ show l
            putStrLn "Not found in db"
            pure $ Left "Not in DB"
        x -> do
           putStrLn "Found in db"
           pure $ Right x

dbCheck :: IO (DbConnection Closed)
dbCheck = do
    conn <- setupDb
    let c = connection conn
    r <- query_ c "SELECT * FROM restaurants" :: IO [Restaurant]
    print r
    print $ length r
    u <- query_ c "SELECT * FROM users" :: IO [Maybe U.User]
    print u
    print $ length u
    v <- query_ c "SELECT * FROM likes" :: IO [Like]
    print v
    print $ length v
    dbClose conn

dbClear :: IO (DbConnection Closed)
dbClear = do
    conn <- setupDb
    let c = connection conn
    execute_ c "DROP TABLE restaurants"
    execute_ c "DELETE FROM users"
    execute_ c "DELETE FROM likes"
    dbClose conn

db :: IO (DbConnection Closed)
db = do
    conn <- setupDb
    let c = connection conn
    _ <- storeInDB
        [ Restaurant
            (Business "ll" "http://" "name" 10 (Just "$$") [Hours True] [Category "hello"])
            (Just "https...")
            (Loc 1 2)
        ]
    _ <- storeInDB
        [ Restaurant
            (Business "lt" "http://" "lame" 10 (Just "4") [Hours True] [Category "hello"])
            (Just "https...")
            (Loc 1 8)
        ]
    b <- query_ c "SELECT * FROM restaurants WHERE latitude > 0 " :: IO [Restaurant]
    print b
    execute_ c "DELETE FROM restaurants WHERE latitude > 0"
    v <- query_ c "SELECT * FROM restaurants WHERE latitude > 0 " :: IO [Restaurant]
    print v
    dbClose conn
