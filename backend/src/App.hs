{-# LANGUAGE DeriveGeneric #-}

module App (server) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
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

data NextError
    = NoMoreRestaurants
    | LookupFailed String

-- | Fetch restaurants near the location from Yelp and store them
yelpAndStore :: Connection -> YelpAPIKey -> Loc -> IO (Either NextError ())
yelpAndStore conn k l = do
    apiRes <- reqRes k l
    case apiRes of
        Left e -> pure $ Left $ LookupFailed $ "Yelp search near " ++ show l ++ " failed: " ++ show e
        Right (Series res) -> Right <$> storeInDB conn res

-- | The restaurant after the given one, or the first if none is given
getRes :: Connection -> YelpAPIKey -> Loc -> Maybe String -> IO (Either NextError Restaurant)
getRes conn k l current = do
    -- Check the DB for this location. If nothing is there ask the API
    cached <- inDB conn l
    stored <-
        if null cached
            then yelpAndStore conn k l >>= traverse (const $ inDB conn l)
            else pure (Right cached)
    case nextAfter current <$> stored of
        Left e -> pure $ Left e
        Right Nothing -> pure $ Left NoMoreRestaurants
        -- Restaurant already has a image URL
        Right (Just res@(Restaurant _ (Just _) _)) -> pure $ Right res
        -- Image URL is missing
        Right (Just (Restaurant b Nothing pos)) -> do
            maybeImg <- reqIMG k $ bid b
            case maybeImg of
                Right i -> do
                    setImage conn (bid b) i
                    pure $ Right (Restaurant b (Just i) pos)
                Left e -> pure $ Left $ LookupFailed $ "Failed to get image for " ++ bid b ++ ": " ++ show e

-- | Body of a /decision request
data DecisionRequest = DecisionRequest
    { user :: !U.UserId
    , restaurant :: !String
    , liked :: !Bool
    }
    deriving (Generic, Show)

instance FromJSON DecisionRequest

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

-- | Record the decision and return the restaurant if it is now a match
decide :: Connection -> U.User -> String -> Bool -> IO (Maybe Restaurant)
decide conn u r l = do
    recordDecision conn $ Decision (U.userId u) r l
    if l then matchedRestaurant conn (U.session u) r else pure Nothing

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

    -- Responds with the restaurant if this decision made it a match, otherwise null
    post "/decision" $ do
        DecisionRequest uid r l <- jsonData
        found <- liftIO $ withConn $ \c -> getUser c uid
        case found of
            Nothing -> errorResponse status404 "Unknown user"
            Just u -> liftIO (withConn $ \c -> decide c u r l) >>= json

    post "/restaurant" $ do
        NextRestaurant l rId <- jsonData
        res <- liftIO $ withConn $ \c -> getRes c (envYelpKey env) l rId
        case res of
            Left NoMoreRestaurants -> errorResponse status404 "No more restaurants nearby"
            Left (LookupFailed e) -> do
                logMsg e
                errorResponse status502 "Could not load restaurants"
            Right r -> json r
