{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
module App (server) where

import Control.Monad.IO.Class (liftIO)
import Data.List (uncons)
import Data.Maybe (listToMaybe)
import GHC.Generics (Generic)
import Network.Wai.Middleware.Cors (
    CorsResourcePolicy (corsMethods, corsOrigins, corsRequestHeaders),
    cors,
    simpleCorsResourcePolicy,
    simpleHeaders,
    simpleMethods,
 )
import System.Environment.Blank (getEnv)
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

import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Db
import Restaurant
import qualified User as U
import Yelp
import Web.Scotty.Trans (pathParam)

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

-- TODO Determine how to propagate these errors rather than using a maybe
yelpAndStore :: YelpAPIKey -> Loc -> IO (Either String [Restaurant])
yelpAndStore k l = do
  apiRes <- reqRes k l
  case apiRes of
    Was (Series []) -> pure $ Left "No restaurant from API"
    Was (Series b) -> do
                let res =  (\x -> Restaurant x Nothing l) <$> b
                _ <- storeInDB res
                pure $ Right res
    x -> pure $ Left $ show x


-- TODO modify to restrict to never be a maybe URL past this point
getRes :: YelpAPIKey -> Loc -> Maybe String -> IO (Either String Restaurant)
getRes k loc restaurantId = do
    -- Check the DB for this location. If nothing is there ask the API
    maybeDbRes <- inDB loc >>= either (const $ yelpAndStore k loc) (return . Right)
    case maybeDbRes of
        Left e -> pure $ Left $ "Failed to: get restaurants at location " ++ show loc ++ "with error " ++ e
        Right dbr -> do
            let r = case restaurantId of
                      Just rid -> case uncons . dropWhile (\res -> bid (bus res) /= rid) $ dbr of
                                    Just (_, x:_) -> Just x
                                    Just (_, []) -> Nothing
                                    Nothing -> Nothing
                      Nothing ->  listToMaybe dbr
            case r of
                Nothing -> pure $ Left $ "No restaurants came back from the db" <> show dbr
                -- Restaurant already has a image URL
                Just res@(Restaurant _ (Just _) _) -> pure $ Right res
                -- Image URL is missing
                Just (Restaurant b Nothing l) -> do
                    maybeImg <- reqIMG k $ bid b
                    case maybeImg of
                        Was i -> do
                            _ <- addImgToDb (Restaurant b (Just i) l)
                            pure $ Right (Restaurant b (Just i) l)
                        _ -> pure $ Left $ "Failed to get restaurants image " ++ show maybeImg

data RestaurantDecision = RestaurantDecision
    { id :: !String
    , likedRestaurant :: !(Maybe String)
    }
    deriving (Generic, Show)

instance FromJSON RestaurantDecision

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
                liftIO $ print "hello"
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
                liftIO $ print (d :: RestaurantDecision)
                case d of
                    (RestaurantDecision i (Just l)) -> do
                        r <- liftIO $ do
                                    _ <- addLikeToDb $ Db.Like i l
                                    (_, maybeU) <- getUser i
                                    case maybeU of
                                      ((Just (U.User _ s)):_) -> do
                                                        putStr "getting matches"
                                                        (_, matches) <- likedRestaurants s
                                                        print $ "matches where" <> show matches
                                                        pure $ listToMaybe matches
                                      (Nothing:_) -> pure Nothing
                                      [] -> pure Nothing
                        liftIO $ print $ " sending " <> show r
                        json (r :: Maybe Restaurant)
                    _ -> json (Nothing :: Maybe Restaurant)

            options "/restaurant" $ text "success"
            post "/restaurant" $ do
                liftIO $ print "Getting res"
                restaurantReq <- jsonData
                case restaurantReq of
                    Nothing -> fail "bad"
                    (Just (NextRestaurant l rId)) -> do
                        liftIO $ putStrLn "Getting res"
                        liftIO $ print l
                        eitherRes <- liftIO $ getRes k l rId
                        liftIO $ print eitherRes
                        case eitherRes of
                            Left e -> fail e
                            Right r -> json r
