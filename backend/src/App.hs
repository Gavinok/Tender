{-# LANGUAGE DeriveGeneric #-}

module App (server) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Database.SQLite.Simple (Connection)
import qualified Data.Text as T
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
    queryParam,
    queryParamMaybe,
    redirect,
    scotty,
    setHeader,
    status,
 )

import Data.Aeson (FromJSON, object, (.=))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types (Status, status400, status404, status502)
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.IO (hPutStrLn, stderr)

import Db
import OpenStreetMap (openStreetMap)
import Places
import Provider (Provider (..), searchWithFallback)
import Restaurant (Loc)
import qualified User as U
import Yelp (YelpKey (..), yelp)

-- | Configuration shared by every request
data Env = Env
    { envDbPath :: FilePath
    , -- | Tried in order until one finds restaurants
      envProviders :: [Provider]
    , -- | Needed to show Google's photos
      envPlacesKey :: Maybe PlacesKey
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

-- | Body of a /decision request
data DecisionRequest = DecisionRequest
    { user :: !U.UserId
    , restaurant :: !Text
    , liked :: !Bool
    }
    deriving (Generic, Show)

instance FromJSON DecisionRequest

{- | Restaurant sources in the order given by TENDER_SOURCES (default
google,yelp,openstreetmap), leaving out the ones without a key
-}
providersFromEnv :: Maybe PlacesKey -> Maybe YelpKey -> String -> IO [Provider]
providersFromEnv placesKey yelpKey order = concat <$> traverse provider (T.splitOn "," (T.pack order))
  where
    provider name = case (T.strip name, placesKey, yelpKey) of
        ("google", Just k, _) -> pure [googlePlaces k]
        ("google", Nothing, _) -> skip "Google Places" "GOOGLE_PLACES_KEY"
        ("yelp", _, Just k) -> pure [yelp k]
        ("yelp", _, Nothing) -> skip "Yelp" "YELP_KEY"
        ("openstreetmap", _, _) -> pure [openStreetMap]
        (other, _, _) -> fail ("Unknown source in TENDER_SOURCES: " ++ T.unpack other)
    skip label var = do
        logMsg $ "Not using " ++ label ++ " since " ++ var ++ " is not set"
        pure []

server :: IO ()
server = do
    let key = fmap (TE.encodeUtf8 . T.pack)
    placesKey <- fmap PlacesKey . key <$> getEnv "GOOGLE_PLACES_KEY"
    yelpKey <- fmap YelpKey . key <$> getEnv "YELP_KEY"
    order <- fromMaybe "google,yelp,openstreetmap" <$> getEnv "TENDER_SOURCES"
    providers <- providersFromEnv placesKey yelpKey order
    logMsg $ "Restaurant sources: " ++ T.unpack (T.intercalate ", " (map providerName providers))
    dbPath <- fromMaybe "test.db" <$> getEnv "TENDER_DB"
    initDb dbPath
    app (Env dbPath providers placesKey)

logMsg :: MonadIO m => String -> m ()
logMsg = liftIO . hPutStrLn stderr

-- | Respond with an error status and a message the frontend can show
errorResponse :: Status -> Text -> ActionM ()
errorResponse s msg = status s >> json (object ["error" .= msg])

-- | Record the decision and whether it made the restaurant a match
decide :: Connection -> U.User -> Text -> Bool -> IO Bool
decide conn u r l = do
    recordDecision conn $ Decision (U.userId u) r l
    if l then isMatch conn (U.session u) r else pure False

-- | Width to ask Google for. Cards are at most about 450 css pixels wide.
photoWidth :: Int
photoWidth = 1200

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
                -- Set when this browser has been in the session before
                returning <- (>>= U.userFromText) <$> queryParamMaybe "user"
                uid <- liftIO $ withConn $ \c -> joinSession c s returning
                json uid

    -- Restaurants near the location, fresh for each session
    post "/restaurants" $ do
        l <- jsonData :: ActionM Loc
        found <- liftIO $ searchWithFallback logMsg (envProviders env) l
        case found of
            Nothing -> errorResponse status502 "Couldn't get restaurants from any source"
            Just rs -> json rs

    -- Redirects to the photo with the given resource name, so the key stays here
    get "/photo" $ do
        photo <- queryParam "name"
        case envPlacesKey env of
          Nothing -> errorResponse status404 "Google photos need GOOGLE_PLACES_KEY"
          Just key
            | not (isPhotoName photo) -> errorResponse status400 "Not a photo name"
            | otherwise -> do
                found <- liftIO $ photoUri key photo photoWidth
                case found of
                    Left e -> do
                        logMsg $ "Photo " ++ T.unpack photo ++ " failed: " ++ show e
                        errorResponse status502 "Couldn't get the photo from Google"
                    Right uri -> do
                        -- Lets the browser reuse it for the rest of the session
                        setHeader "Cache-Control" "private, max-age=3600"
                        redirect (TL.fromStrict uri)

    post "/decision" $ do
        DecisionRequest uid r l <- jsonData
        found <- liftIO $ withConn $ \c -> getUser c uid
        case found of
            Nothing -> errorResponse status404 "Unknown user"
            Just u -> do
                match <- liftIO $ withConn $ \c -> decide c u r l
                json (object ["match" .= match])
