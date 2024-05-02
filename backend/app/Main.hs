{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.List (uncons)
import Database.SQLite.Simple (
    FromRow (..),
    ToRow (..)
 )
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
    middleware,
    options,
    post,
    scotty,
    text,
 )

import Data.Aeson (FromJSON)

-- import Data.ByteString.Char8 (putStrLn)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Db
import Restaurant
import System.Random
import qualified User as U
import Yelp
import Control.Applicative ((<|>))
import Web.Scotty.Trans (pathParam)

-- Something like that maybe ?
-- sessionId :: Int -> (UUID, StdGen)
-- sessionId seed = random $ mkStdGen seed

sessionId :: IO UUID
sessionId = nextRandom

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

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- TODO Determin how to propigate these errors rather than using a maybe
yelpAndStore :: YelpAPIKey -> Loc -> IO (Either String [Resturant])
yelpAndStore k l = do
  apiRes <- reqRes k l
  case apiRes of
    Was (Series []) -> pure $ Left "No restaurant from API"
    Was (Series b) -> do
                let res =  (\x -> Resturant x Nothing l) <$> b
                _ <- storeInDB res
                pure $ Right res
    x -> pure $ Left $ show x


-- TODO modify to restrict to never be a maybe URL past this point
getRes :: YelpAPIKey -> Loc -> Maybe String -> IO (Either String Resturant)
getRes k loc resturantId = do
    maybeDbRes <- do inDB loc >>= either (\x -> yelpAndStore k loc) (return . Right)
    case maybeDbRes of
        Left e -> pure $ Left $ "Failed to: get restaurants at location " ++ show loc ++ "with error " ++ e
        Right dbr -> do
            let r = case resturantId of
                      Just rid -> case uncons . dropWhile (\res -> bid (bus res) /= rid) $ dbr of
                                    Just (_, x:_) -> Just x
                                    Just (_, []) -> Nothing
                                    Nothing -> Nothing
                      Nothing ->  safeHead dbr
            case r of
                Nothing -> pure $ Left $ "No restaurants came back from the db" <> show dbr
                Just ressy@(Resturant _ (Just _) _) -> pure $ Right ressy
                Just (Resturant b Nothing l) -> do
                    maybeImg <- reqIMG k $ bid b
                    case maybeImg of
                        Was i -> do
                            _ <- addImgToDb (Resturant b (Just i) l)
                            pure $ Right (Resturant b (Just i) l)
                        _ -> pure $ Left $ "Failed to get restaurants image " ++ show maybeImg

data ResturantDecision = ResturantDecision
    { id :: !String
    , likedResturant :: !(Maybe String)
    }
    deriving (Generic, Show)
    deriving anyclass (ToRow)

deriving anyclass instance FromRow ResturantDecision

instance FromJSON ResturantDecision

type Match = Business

firstElem :: [a] -> Maybe a
firstElem xs = case xs of
  [] -> Nothing
  (x:_) -> Just x

-- curl -X POST -d"{\"latitude\": 48, \"longitude\": -122}" \
--             http://localhost:5000/hello/gavin
server :: IO ()
server = do
    keyMaybe <- getEnv "YELP_KEY"
    case keyMaybe of
        Nothing -> fail "Failed to find Yelp Key"
        Just key -> scotty 5001 $ do
            let k = YelpAPIKey key
            middleware (cors $ const $ Just apiCors)

            options "/newsession" $ text "success"
            get "/newsession" $ do
                liftIO $ putStrLn "Getting Session"
                -- uid <- liftIO sessionId
                session <- liftIO sessionId
                -- liftIO $ addUserToDb (U.User uid session)
                json session

            options "/addtosession/:session" $ text "success"
            get "/addtosession/:session" $ do
                                    liftIO $ putStrLn "Adding To Session"
                                    sessionString <- pathParam "session"
                                    -- liftIO $ putStrLn "Adding To Session " + sessionString
                                    uid <- liftIO sessionId
                                    let session = U.fromText sessionString
                                    case session of
                                      Nothing -> json uid
                                      Just s -> do _ <- liftIO $ addUserToDb (U.User uid s)
                                                   json uid

            options "/liked" $ text "success"
            post "/liked" $ do
                d <- jsonData
                liftIO $ print (d :: ResturantDecision)
                case d of
                    (ResturantDecision i (Just l)) -> do
                        r <- liftIO $ do
                                    _ <- addLikeToDb $ Db.Like i l
                                    (_, maybeU) <- getUser i
                                    case maybeU of
                                      ((Just (U.User _ s)):_) -> do
                                                        putStr "getting matches"
                                                        (_, matches) <- likedResturants s
                                                        print $ "matches where" <> show matches
                                                        pure $ case matches of
                                                          [x] -> Just x
                                                          (x:_) -> Just x
                                                          [] -> Nothing
                                      (Nothing:_) -> pure Nothing
                                      [] -> pure Nothing
                        liftIO $ print $ " sending " <> show r
                        json (r :: Maybe Resturant)
                    _ -> json (Nothing :: Maybe Resturant)

            options "/resturant" $ text "success"
            post "/resturant" $ do
                liftIO $ print "Getting res"
                resturantReq <- jsonData
                case resturantReq of
                    Nothing -> fail "bad"
                    (Just (NextResturant l rId)) -> do
                        liftIO $ putStrLn "Getting res"
                        liftIO $ print l
                        eitherRes <- liftIO $ getRes k l rId
                        liftIO $ print eitherRes
                        case eitherRes of
                            Left e -> fail e
                            Right r -> json r
main :: IO ()
main = server
