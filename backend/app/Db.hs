{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Db (addImgToDb, storeInDB, inDB, setupDb, addUserToDb, addLikeToDb, likedResturants, LikedResturant(..), Like (..), dbClear, getUser) where

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
            [ "CREATE TABLE IF NOT EXISTS resturants "
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
            , "(userId TEXT, resturantId TEXT)"
            ]
    pure conn

addImgToDb :: Resturant -> IO (DbConnection Closed)
addImgToDb (Resturant b (Just i) _) = do
    putStrLn $ "adding image for " ++ show (bid b)
    conn <- setupDb
    execute (connection conn) "UPDATE resturants SET imagelink = ? WHERE id = ?" (Just i, bid b)
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
    { userId :: String
    , resturantId :: String
    }
    deriving (Show, Generic)
    deriving anyclass (ToRow)
deriving anyclass instance FromRow Like

addLikeToDb :: Like -> IO (DbConnection Closed)
addLikeToDb (Like u r) = do
    putStrLn $ "adding Like for User " ++ show u ++ " Resturant " ++ show r
    conn <- setupDb
    execute (connection conn) "INSERT INTO likes (userId, resturantId) VALUES (?, ?)" (u, r)
    dbClose conn

newtype LikedResturant = LikedResturant {toString :: String} deriving (Generic, Show)
deriving anyclass instance FromRow LikedResturant

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

likedResturants :: U.Session -> IO (DbConnection Closed, [Resturant])
likedResturants s = do
    conn <- setupDb
    r <-
        query
            (connection conn)
            ( mconcat
                [
                 "WITH l AS (SELECT DISTINCT likes.userId, likes.resturantId FROM likes) "
                , "SELECT resturants.id, resturants.url, resturants.name, resturants.rating, resturants.price, resturants.hours, resturants.category, resturants.imagelink, resturants.latitude, resturants.longitude "
                , "FROM users JOIN l "
                , "ON users.userId IS l.userId "
                , "JOIN resturants "
                , "ON l.resturantId is resturants.id "
                , "WHERE users.session = ? "
                , "GROUP BY l.resturantId "
                , "HAVING count(l.resturantId) > 1 "
                ]
            )
            [s]
    c <- dbClose conn
    pure (c, r)

storeInDB :: [Resturant] -> IO (DbConnection Closed)
storeInDB b = do
    conn <- setupDb
    mapM_
        (execute (connection conn) "INSERT OR IGNORE INTO resturants (id, url, name, rating, price, hours, category, imagelink, latitude, longitude) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
        b
    dbClose conn

-- TODO Use freemonads to mark this DB connection as needing to be closed
inDB :: Loc -> IO (Maybe [Resturant])
inDB l = do
    conn <- setupDb
    r <- query (connection conn) "SELECT * FROM resturants Where latitude is ? and longitude is ?" (latitude l, longitude l) :: IO [Resturant]
    dbClose conn
    pure $ case r of
        [] -> Nothing
        x -> Just x
dbCheck :: IO (DbConnection Closed)
dbCheck = do
    conn <- setupDb
    let c = connection conn
    r <- query_ c "SELECT * FROM resturants" :: IO [Resturant]
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
    execute_ c "DROP TABLE resturants"
    execute_ c "DELETE FROM users"
    execute_ c "DELETE FROM likes"
    dbClose conn

db :: IO (DbConnection Closed)
db = do
    conn <- setupDb
    let c = connection conn
    _ <- storeInDB
        [ Resturant
            (Business "ll" "http://" "name" 10 (Just "$$") [Hours True] [Category "hello"])
            (Just "https...")
            (Loc 1 2)
        ]
    _ <- storeInDB
        [ Resturant
            (Business "lt" "http://" "lame" 10 (Just "4") [Hours True] [Category "hello"])
            (Just "https...")
            (Loc 1 8)
        ]
    b <- query_ c "SELECT * FROM resturants WHERE latitude > 0 " :: IO [Resturant]
    print b
    execute_ c "DELETE FROM resturants WHERE latitude > 0"
    v <- query_ c "SELECT * FROM resturants WHERE latitude > 0 " :: IO [Resturant]
    print v
    dbClose conn
