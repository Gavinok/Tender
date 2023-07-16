{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Db (addImgToDb, storeInDB, inDB, setupDb, addUserToDb, addLikeToDb, Like (..)) where

import Data.UUID (UUID, toText)
import Database.SQLite.Simple (
    Connection,
    FromRow,
    ToRow,
    close,
    execute,
    execute_,
    field,
    fromRow,
    open,
    query,
    query_,
    toRow,
 )
import Database.SQLite.Simple.ToField (ToField (toField))
import GHC.Generics (Generic)
import Restaurant
import qualified User as U

newtype ClosedDbConnection = ClosedDbConnection () deriving (Show)
newtype InitializedDb = InitializedDb Connection

dbClose :: Connection -> IO ClosedDbConnection
dbClose conn = do
    close conn
    pure $ ClosedDbConnection ()

setupDb :: IO InitializedDb
setupDb = do
    conn <- open "test.db"
    let ex = execute_ conn
    -- Restaurants
    ex $
        mconcat
            [ "CREATE TABLE IF NOT EXISTS resturants "
            , "(id TEXT PRIMARY KEY, url TEXT, name Text, rating FLOAT, price TEXT, hours INTEGER,"
            , " imagelink TEXT,"
            , " latitude FLOAT, longitude FLOAT)"
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
    pure $ InitializedDb conn

addImgToDb :: Resturant -> IO ClosedDbConnection
addImgToDb (Resturant b (Just i) _) = do
    putStrLn $ "adding image for " ++ show (bid b)
    (InitializedDb conn) <- setupDb
    execute conn "UPDATE resturants SET imagelink = ? WHERE id = ?" (Just i, bid b)
    dbClose conn
addImgToDb _ = fail "invalid input to update"

addUserToDb :: U.User -> IO ClosedDbConnection
addUserToDb (U.User i s) = do
    (InitializedDb conn) <- setupDb
    execute conn "INSERT INTO users (userId, session) VALUES (?, ?)" (i, s)
    putStrLn $ "adding User " ++ show i ++ " with session " ++ (show s)
    dbClose conn
addUserToDb _ = fail "invalid input to update"

data Like = Like
    { userId :: String
    , resturantId :: String
    }
    deriving (Show, Generic)
    deriving anyclass (ToRow)
deriving anyclass instance FromRow Like

addLikeToDb :: Like -> IO ClosedDbConnection
addLikeToDb (Like u r) = do
    putStrLn $ "adding Like for User " ++ show u ++ " Resturant " ++ show r
    (InitializedDb conn) <- setupDb
    execute conn "INSERT INTO likes (userId, resturantId) VALUES (?, ?)" (u, r)
    dbClose conn

newtype LikedResturant = LikedResturant {toString :: String} deriving (Generic, Show)
deriving anyclass instance FromRow LikedResturant

likedResturants :: U.Session -> IO (ClosedDbConnection, [LikedResturant])
likedResturants s = do
    (InitializedDb conn) <- setupDb
    r <-
        query
            conn
            ( mconcat
                [ "SELECT resturantId "
                , "FROM users JOIN likes "
                , "ON users.userId = likes.userId "
                , "JOIN resturants "
                , "ON likes.resturantId = resturants.id "
                , "WHERE session = ?"
                ]
            )
            (s) ::
            IO [LikedResturant]
    c <- (dbClose conn)
    pure (c, r)

storeInDB :: [Resturant] -> IO ClosedDbConnection
storeInDB b = do
    (InitializedDb conn) <- setupDb
    mapM_
        (execute conn "INSERT INTO resturants (id, url, name, rating, price, hours, imagelink, latitude, longitude) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)")
        b
    dbClose conn

-- TODO Use freemonads to mark this DB connection as needing to be closed
inDB :: Loc -> IO (Maybe [Resturant])
inDB l = do
    (InitializedDb conn) <- setupDb
    r <- query conn "SELECT * FROM resturants Where latitude is ? and longitude is ?" (latitude l, longitude l) :: IO [Resturant]
    close conn
    pure $ case r of
        [] -> Nothing
        x -> Just x

dbCheck :: IO ClosedDbConnection
dbCheck = do
    (InitializedDb conn) <- setupDb
    r <- query_ conn "SELECT * FROM resturants" :: IO [Resturant]
    print r
    print $ length r
    u <- query_ conn "SELECT * FROM users" :: IO [Maybe U.User]
    print u
    print $ length u
    v <- query_ conn "SELECT * FROM likes" :: IO [Like]
    print v
    print $ length v
    dbClose conn

dbClear :: IO ()
dbClear = do
    (InitializedDb conn) <- setupDb
    execute_ conn "DELETE FROM resturants WHERE latitude > 0"
    execute_ conn "DELETE FROM users"
    execute_ conn "DELETE FROM likes"
    close conn

db :: IO ()
db = do
    (InitializedDb conn) <- setupDb
    storeInDB [Resturant (Business "ll" "http://" "name" 10 (Just "$$") [Hours True]) (Just "https...") (Loc 1 2)]
    storeInDB [Resturant (Business "lt" "http://" "lame" 10 (Just "4") [Hours True]) (Just "https...") (Loc 1 8)]
    b <- query_ conn "SELECT * FROM resturants WHERE latitude > 0 " :: IO [Resturant]
    print b
    execute_ conn "DELETE FROM resturants WHERE latitude > 0"
    v <- query_ conn "SELECT * FROM resturants WHERE latitude > 0 " :: IO [Resturant]
    print v
    close conn
