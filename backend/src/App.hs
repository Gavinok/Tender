{-# LANGUAGE DeriveGeneric #-}

module App (server) where

import Control.Monad.IO.Class (liftIO)
import Data.List (uncons)
import Data.Maybe (fromMaybe, listToMaybe)
import Database.SQLite.Simple (Connection)
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
    pathParam,
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

-- | Configuration shared by every request
data Env = Env
    { envDbPath :: FilePath
    , envYelpKey :: YelpAPIKey
    }

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
yelpAndStore :: Connection -> YelpAPIKey -> Loc -> IO (Either String [Restaurant])
yelpAndStore conn k l = do
  apiRes <- reqRes k l
  case apiRes of
    Was (Series []) -> pure $ Left "No restaurant from API"
    Was (Series b) -> do
                let res =  (\x -> Restaurant x Nothing l) <$> b
                storeInDB conn res
                pure $ Right res
    x -> pure $ Left $ show x


-- TODO modify to restrict to never be a maybe URL past this point
getRes :: Connection -> YelpAPIKey -> Loc -> Maybe String -> IO (Either String Restaurant)
getRes conn k loc restaurantId = do
    -- Check the DB for this location. If nothing is there ask the API
    cached <- inDB conn loc
    maybeDbRes <- if null cached then yelpAndStore conn k loc else pure (Right cached)
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
                            setImage conn (bid b) i
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
    dbPath <- fromMaybe "test.db" <$> getEnv "TENDER_DB"
    case keyMaybe of
        Nothing -> fail "Failed to find Yelp Key"
        Just key -> do
            initDb dbPath
            app (Env dbPath (YelpAPIKey key))

app :: Env -> IO ()
app env = scotty 5001 $ do
            let k = envYelpKey env
                withConn :: (Connection -> IO a) -> IO a
                withConn = withDb (envDbPath env)
            middleware (cors $ const $ Just apiCors)

            options "/newsession" $ text "success"
            get "/newsession" $ do
                liftIO $ putStrLn "Getting Session"
                session <- liftIO sessionId
                liftIO $ print "hello"
                json session

            options "/addtosession/:session" $ text "success"
            get "/addtosession/:session" $ do
                                    liftIO $ putStrLn "Adding To Session"
                                    sessionString <- pathParam "session"
                                    uid <- liftIO sessionId
                                    let session = U.fromText sessionString
                                    case session of
                                      Nothing -> json uid
                                      Just s -> do liftIO $ withConn $ \c -> addUserToDb c (U.User uid s)
                                                   json uid

            options "/liked" $ text "success"
            post "/liked" $ do
                d <- jsonData
                liftIO $ print (d :: RestaurantDecision)
                case d of
                    (RestaurantDecision i (Just l)) -> do
                        r <- liftIO $ withConn $ \c -> do
                                    addLikeToDb c $ Db.Like i l
                                    maybeU <- getUser c i
                                    case maybeU of
                                      Just (U.User _ s) -> do
                                                        putStr "getting matches"
                                                        matches <- likedRestaurants c s
                                                        print $ "matches where" <> show matches
                                                        pure $ listToMaybe matches
                                      Nothing -> pure Nothing
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
                        eitherRes <- liftIO $ withConn $ \c -> getRes c k l rId
                        liftIO $ print eitherRes
                        case eitherRes of
                            Left e -> fail e
                            Right r -> json r
