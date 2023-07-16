{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Database.SQLite.Simple (
    FromRow (..),
    ToRow (..),
    field,
 )
import Database.SQLite.Simple.ToField (ToField (toField))
import GHC.Generics (Generic)
import Network.Wai.Middleware.Cors (
    CorsResourcePolicy (corsMethods, corsOrigins, corsRequestHeaders),
    cors,
    simpleCorsResourcePolicy,
    simpleHeaders,
    simpleMethods,
 )
import System.Environment.Blank (getEnv)
import System.Random (
    Random (randomR),
    RandomGen,
    StdGen,
    getStdRandom,
 )
import Web.Scotty (
    get,
    json,
    jsonData,
    liftAndCatchIO,
    middleware,
    options,
    post,
    scotty,
    text,
 )

import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import Data.ByteString (putStr)

-- import Data.ByteString.Char8 (putStrLn)
import qualified Data.ByteString.Lazy as L
import Data.UUID (UUID, fromText, toText)
import Data.UUID.V4 (nextRandom)
import Db
import qualified GHC.Base as Data.ByteString
import Restaurant
import System.Random
import qualified User as U
import Yelp

-- Something like that maybe ?
sessionId :: Int -> (UUID, StdGen)
sessionId seed = random $ mkStdGen seed

-- sessionId :: IO UUID
-- sessionId = nextRandom

{- | A preset version of cors with added OPTIONS method and
   Content-Type, Authorization headers
-}
apiCors :: CorsResourcePolicy
apiCors =
    simpleCorsResourcePolicy
        { corsOrigins = Nothing
        , corsMethods = simpleMethods <> ["OPTIONS", "PATCH"]
        , corsRequestHeaders = simpleHeaders
        }

randomIndex :: [a] -> IO a
randomIndex lst = do
    newnum <- getStdRandom $ randomR (0, Prelude.length lst - 1)
    putStrLn $ "Looking this up now" ++ show newnum
    pure $ lst !! abs newnum

-- TODO modify to restrict to never be a maybe URL past this point
getRes :: YelpAPIKey -> Loc -> IO (Either String Resturant)
getRes k loc = do
    maybeDbRes <- inDB loc
    case maybeDbRes of
        Just dbr -> do
            putStrLn "Found in db"
            r <- randomIndex dbr
            case r of
                (Resturant _ (Just _) _) -> pure $ Right r
                (Resturant b Nothing l) -> do
                    putStrLn "Looking up"
                    maybeImg <- reqIMG k $ bid b
                    case maybeImg of
                        Was i -> do
                            addImgToDb (Resturant b (Just i) l)
                            pure $ Right (Resturant b (Just i) l)
                        _ -> pure $ Left $ "Failed to get resturants image " ++ show maybeImg
        Nothing -> do
            apiRes <- reqRes k loc
            case apiRes of
                Was (Series res) -> do
                    storeInDB $ (\x -> Resturant x Nothing loc) <$> res
                    b <- randomIndex res
                    maybeImg <- reqIMG k $ bid b
                    case maybeImg of
                        Was i -> do
                            let r = Resturant b (Just i) loc
                            addImgToDb r
                            pure $ Right r
                        _ -> pure $ Left $ "Failed to get resturants image " ++ show maybeImg
                _ -> pure $ Left $ "Failed to: get resturants at location " ++ show loc ++ "with error " ++ show apiRes

data ResturantDecision = ResturantDecision
    { id :: String
    , likedResturant :: Maybe String
    }
    deriving (Generic, Show)
    deriving anyclass (ToRow)

deriving anyclass instance FromRow ResturantDecision

instance FromJSON ResturantDecision

type Match = Business

addLiked :: ResturantDecision -> U.User -> IO ()
addLiked pick user = undefined

compareLiked :: U.Session -> IO [Match]
compareLiked sesh = undefined

wasMatch :: ResturantDecision -> U.User -> IO (Maybe Match)
wasMatch pick usr = do
    addLiked pick usr
    matches <- compareLiked $ U.session usr
    pure $ case matches of
        [] -> Nothing
        (x : _) -> Just x

-- curl -X POST -d"{\"latitude\": 48, \"longitude\": -122}" \
--             http://localhost:5000/hello/gavin
server :: IO ()
server = do
    keyMaybe <- getEnv "YELP_KEY"
    case keyMaybe of
        Nothing -> fail "Failed to find Yelp Key"
        Just key -> scotty 5000 $ do
            let k = YelpAPIKey key
            middleware (cors $ const $ Just apiCors)

            options "/newsession" $ text "success"
            get "/newsession" $ do
                liftIO $ putStrLn "Getting Session"
                let (uid, _) = sessionId 10
                    (session, _) = sessionId 11
                liftIO $ addUserToDb (U.User uid session)
                json uid

            options "/liked" $ text "success"
            post "/liked" $ do
                d <- jsonData
                liftIO $ print (d :: ResturantDecision)
                case d of
                    (ResturantDecision i (Just l)) -> do
                        liftAndCatchIO $
                            addLikeToDb $
                                (Db.Like i l)
                        json (Just l)
                    _ -> json (Nothing :: Maybe Resturant)

            options "/gavin" $ text "success"
            post "/gavin" $ do
                liftIO $ print "Getting res"
                loc <- jsonData
                case loc of
                    Nothing -> fail "bad"
                    (Just l) -> do
                        liftAndCatchIO $ putStrLn "Getting res"
                        liftAndCatchIO $ print l
                        eitherRes <- liftAndCatchIO $ getRes k l
                        case eitherRes of
                            Left e -> fail e
                            Right r -> json r
main :: IO ()
main = server
