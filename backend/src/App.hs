{-# LANGUAGE TupleSections #-}
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
    file,
    notFound,
    request,
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

import Data.Aeson (FromJSON (parseJSON), Value, object, withObject, (.:), (.:?), (.=))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types (Status, status400, status404, status502)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Control.Applicative ((<|>))
import Network.Wai (rawPathInfo)
import System.Directory (doesFileExist)
import System.FilePath (takeExtension, (</>))
import System.IO (hPutStrLn, stderr)

import Db
import OpenStreetMap (openStreetMap)
import Places
import Provider (Provider (..), searchWithFallback)
import Data.List (partition)
import Restaurant (Loc, Restaurant)
import qualified Restaurant as R
import qualified User as U
import Vibe (Chip, Search (..), chipFromId, chipId, maxRadius)
import Yelp (YelpKey (..), yelp)

-- | Configuration shared by every request
data Env = Env
    { envDbPath :: FilePath
    , -- | Tried in order until one finds restaurants
      envProviders :: [Provider]
    , -- | Needed to show Google's photos
      envPlacesKey :: Maybe PlacesKey
    , -- | The built frontend to serve, when set (in production)
      envStatic :: Maybe FilePath
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

-- | Body of a /restaurants request: who is asking, and where they are
data RestaurantsRequest = RestaurantsRequest
    { searcher :: !U.UserId
    , loc :: !Loc
    , -- | Set by "search further": the radius the person was shown, to double
      widenFrom :: !(Maybe Double)
    }
    deriving (Generic, Show)

instance FromJSON RestaurantsRequest where
    parseJSON = withObject "RestaurantsRequest" $ \o ->
        RestaurantsRequest <$> o .: "user" <*> o .: "loc" <*> o .:? "widenFrom"

-- | Body of a POST /vibe request
newtype VibeRequest = VibeRequest [Text]

instance FromJSON VibeRequest where
    parseJSON = withObject "VibeRequest" $ \o -> VibeRequest <$> o .: "chips"

vibeJSON :: ([Chip], Bool) -> Value
vibeJSON (chips, locked) = object ["chips" .= map chipId chips, "locked" .= locked]

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
    -- Keys pasted into a secret or env var often end with a newline
    let key = fmap (TE.encodeUtf8 . T.strip . T.pack)
    placesKey <- fmap PlacesKey . key <$> getEnv "GOOGLE_PLACES_KEY"
    yelpKey <- fmap YelpKey . key <$> getEnv "YELP_KEY"
    order <- fromMaybe "google,yelp,openstreetmap" <$> getEnv "TENDER_SOURCES"
    providers <- providersFromEnv placesKey yelpKey order
    logMsg $ "Restaurant sources: " ++ T.unpack (T.intercalate ", " (map providerName providers))
    dbPath <- fromMaybe "test.db" <$> getEnv "TENDER_DB"
    -- Cloud Run tells the server its port through PORT
    port <- maybe 5001 read <$> ((<|>) <$> getEnv "TENDER_PORT" <*> getEnv "PORT")
    static <- getEnv "TENDER_STATIC"
    initDb dbPath
    app port (Env dbPath providers placesKey static)

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

-- | Restaurants someone else in the session liked first, otherwise in the source's order
likedFirst :: [Text] -> [Restaurant] -> [Restaurant]
likedFirst ids rs = uncurry (<>) (partition ((`elem` ids) . R.restaurantId) rs)

-- | Width to ask Google for. Cards are at most about 450 css pixels wide.
photoWidth :: Int
photoWidth = 1200

app :: Int -> Env -> IO ()
app port env = scotty port $ do
    let withConn :: (Connection -> IO a) -> IO a
        withConn = withDb (envDbPath env)
    middleware logStdout
    -- Also answers CORS preflight requests
    middleware (cors $ const $ Just apiCors)

    get "/api/newsession" $
        liftIO U.newSessionId >>= json

    get "/api/addtosession/:session" $ do
        sessionText <- pathParam "session"
        case U.sessionFromText sessionText of
            Nothing -> errorResponse status400 "Invalid session"
            Just s -> do
                -- Set when this browser has been in the session before
                returning <- (>>= U.userFromText) <$> queryParamMaybe "user"
                uid <- liftIO $ withConn $ \c -> joinSession c s returning
                json uid

    -- The session's vibe chips, and whether swiping has started and locked them
    get "/api/vibe/:session" $ do
        sessionText <- pathParam "session"
        case U.sessionFromText sessionText of
            Nothing -> errorResponse status400 "Invalid session"
            Just s -> liftIO (withConn $ \c -> getVibe c s) >>= json . vibeJSON

    post "/api/vibe/:session" $ do
        sessionText <- pathParam "session"
        VibeRequest ids <- jsonData
        case (U.sessionFromText sessionText, traverse chipFromId ids) of
            (Nothing, _) -> errorResponse status400 "Invalid session"
            (_, Nothing) -> errorResponse status400 "Unknown vibe"
            (Just s, Just chips) -> liftIO (withConn $ \c -> setVibe c s chips) >>= json . vibeJSON

    -- Restaurants near the session's location, fresh for each person
    post "/api/restaurants" $ do
        RestaurantsRequest uid here wider <- jsonData
        known <- liftIO $ withConn $ \c -> getUser c uid
        case known of
            Nothing -> errorResponse status404 "Unknown user"
            Just u -> do
                (l, (chips, _), radius, decided) <- liftIO $ withConn $ \c -> do
                    let s = U.session u
                    l <- sessionLocation c s here
                    radius <- maybe (sessionRadius c s) (widenSearch c s) wider
                    (l,,radius,) <$> getVibe c s <*> decidedBy c uid
                -- Searching further only needs the ring outside what the person saw
                let inner = maybe 0 (min radius) wider
                found <- liftIO $ searchWithFallback logMsg (envProviders env) (Search l chips radius inner)
                case found of
                    Nothing -> errorResponse status502 "Couldn't get restaurants from any source"
                    Just rs -> do
                        others <- liftIO $ withConn $ \c -> othersLiked c u
                        json $
                            object
                                [ -- Leaving out ones this person already decided on, e.g. before a reload
                                  "restaurants" .= likedFirst others (filter ((`notElem` decided) . R.restaurantId) rs)
                                , "othersLiked" .= others
                                , "vibe" .= map chipId chips
                                , "radius" .= radius
                                , "maxRadius" .= maxRadius
                                ]

    -- Redirects to the photo with the given resource name, so the key stays here
    get "/api/photo" $ do
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

    post "/api/decision" $ do
        DecisionRequest uid r l <- jsonData
        found <- liftIO $ withConn $ \c -> getUser c uid
        case found of
            Nothing -> errorResponse status404 "Unknown user"
            Just u -> do
                (match, others) <- liftIO $ withConn $ \c -> (,) <$> decide c u r l <*> othersLiked c u
                json (object ["match" .= match, "othersLiked" .= others])

    -- Anything else: the frontend's files, and its page for every other path
    -- (such as /<session>) so links work. Only when a frontend is configured.
    notFound $ do
        path <- requestPath
        case envStatic env of
            Just dir | not ("/api/" `T.isPrefixOf` path) -> serveFrontend dir path
            _ -> errorResponse status404 "Not found"

requestPath :: ActionM Text
requestPath = TE.decodeUtf8 . rawPathInfo <$> request

{- | A file from the built frontend, or its index.html when there is no such
file. Paths with . or .. segments are refused, so nothing outside the
directory can be served.
-}
serveFrontend :: FilePath -> Text -> ActionM ()
serveFrontend dir path = do
    let segments = filter (not . T.null) (T.splitOn "/" path)
        safe = all (`notElem` [".", ".."]) segments
        candidate = dir </> T.unpack (T.intercalate "/" segments)
    isFile <- liftIO $ if safe && not (null segments) then doesFileExist candidate else pure False
    if isFile
        then do
            setHeader "Content-Type" (contentType candidate)
            -- Parcel puts a hash in every file name but index.html
            setHeader "Cache-Control" "public, max-age=31536000, immutable"
            file candidate
        else do
            setHeader "Content-Type" "text/html; charset=utf-8"
            setHeader "Cache-Control" "no-cache"
            file (dir </> "index.html")

contentType :: FilePath -> TL.Text
contentType f = case takeExtension f of
    ".html" -> "text/html; charset=utf-8"
    ".js" -> "text/javascript"
    ".css" -> "text/css"
    ".svg" -> "image/svg+xml"
    ".png" -> "image/png"
    ".jpg" -> "image/jpeg"
    ".ico" -> "image/x-icon"
    ".woff2" -> "font/woff2"
    ".woff" -> "font/woff"
    ".json" -> "application/json"
    ".webmanifest" -> "application/manifest+json"
    _ -> "application/octet-stream"
