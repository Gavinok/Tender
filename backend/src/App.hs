{-# LANGUAGE DeriveGeneric #-}

module App (server) where

import Control.Monad.IO.Class (MonadIO, liftIO)
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
    ActionM,
    get,
    json,
    jsonData,
    middleware,
    pathParam,
    post,
    scotty,
    status,
 )

import Data.Aeson (FromJSON, object, (.=))
import Data.Text (Text)
import Network.HTTP.Types (Status, status400, status404, status502)
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.IO (hPutStrLn, stderr)

import Db
import Restaurant
import qualified User as U
import Yelp

-- | Configuration shared by every request
data Env = Env
    { envDbPath :: FilePath
    , envYelpKey :: YelpAPIKey
    }

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

yelpAndStore :: Connection -> YelpAPIKey -> Loc -> IO (Either String [Restaurant])
yelpAndStore conn k l = do
  apiRes <- reqRes k l
  case apiRes of
    Left e -> pure $ Left $ show e
    Right (Series []) -> pure $ Left "No restaurant from API"
    Right (Series res) -> do
                storeInDB conn res
                pure $ Right res


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
                        Right i -> do
                            setImage conn (bid b) i
                            pure $ Right (Restaurant b (Just i) l)
                        Left e -> pure $ Left $ "Failed to get restaurants image " ++ show e

data RestaurantDecision = RestaurantDecision
    { id :: !U.UserId
    , likedRestaurant :: !(Maybe String)
    }
    deriving (Generic, Show)

instance FromJSON RestaurantDecision

server :: IO ()
server = do
    keyMaybe <- getEnv "YELP_KEY"
    dbPath <- fromMaybe "test.db" <$> getEnv "TENDER_DB"
    case keyMaybe of
        Nothing -> fail "Failed to find Yelp Key"
        Just key -> do
            initDb dbPath
            app (Env dbPath (YelpAPIKey key))

logMsg :: MonadIO m => String -> m ()
logMsg = liftIO . hPutStrLn stderr

-- | Respond with an error status and a message the frontend can show
errorResponse :: Status -> Text -> ActionM ()
errorResponse s msg = status s >> json (object ["error" .= msg])

-- | Record the like and return the restaurant if it is now a match
recordLike :: Connection -> U.User -> String -> IO (Maybe Restaurant)
recordLike conn user restaurant = do
    addLikeToDb conn $ Db.Like (U.userId user) restaurant
    matchedRestaurant conn (U.session user) restaurant

app :: Env -> IO ()
app env = scotty 5001 $ do
    let withConn :: (Connection -> IO a) -> IO a
        withConn = withDb (envDbPath env)
    middleware logStdout
    -- Also answers CORS preflight requests
    middleware (cors $ const $ Just apiCors)

    get "/newsession" $
        liftIO U.newSessionId >>= json

    get "/addtosession/:session" $ do
        sessionText <- pathParam "session"
        case U.sessionFromText sessionText of
            Nothing -> errorResponse status400 "Invalid session"
            Just s -> do
                uid <- liftIO U.newUserId
                liftIO $ withConn $ \c -> addUserToDb c (U.User uid s)
                json uid

    post "/liked" $ do
        RestaurantDecision uid liked <- jsonData
        user <- liftIO $ withConn $ \c -> getUser c uid
        case (user, liked) of
            (Nothing, _) -> errorResponse status404 "Unknown user"
            -- Dislikes are not recorded
            (Just _, Nothing) -> json (Nothing :: Maybe Restaurant)
            (Just u, Just r) -> liftIO (withConn $ \c -> recordLike c u r) >>= json

    post "/restaurant" $ do
        NextRestaurant l rId <- jsonData
        res <- liftIO $ withConn $ \c -> getRes c (envYelpKey env) l rId
        case res of
            Left e -> do
                logMsg e
                errorResponse status502 "Could not load restaurants"
            Right r -> json r
