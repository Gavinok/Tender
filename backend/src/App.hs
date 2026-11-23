{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module App (server, application, Env (..), Limits, newLimitsWith) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forever, unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Database.SQLite.Simple (Connection)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Environment.Blank (getEnv)
import Web.Scotty (
    ActionM,
    finish,
    header,
    file,
    notFound,
    request,
    get,
    json,
    jsonData,
    middleware,
    defaultHandler,
    Handler (..),
    pathParam,
    post,
    queryParam,
    queryParamMaybe,
    redirect,
    scotty,
    scottyApp,
    ScottyM,
    setHeader,
    status,
 )

import Data.Aeson (FromJSON (parseJSON), Value, object, withObject, (.:), (.:?), (.=))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types (Status, status200, status400, status404, status409, status413, status429, status502)
import qualified Data.ByteString as BS
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Request (RequestSizeException)
import Network.Wai.Middleware.RequestSizeLimit (defaultRequestSizeLimitSettings, requestSizeLimitMiddleware, setMaxLengthForRequest)
import Network.Wai (Application, rawPathInfo, remoteHost)
import System.Directory (doesFileExist)
import System.FilePath (takeExtension, (</>))
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import Db
import OpenStreetMap (openStreetMap)
import Places
import Limits (Counter, Limit (..), Recent, newCounter, newRecent, remember, seenRecently, trySpend)
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
    , envLimits :: Limits
    }

{- | Caps on what costs money. The site-wide daily caps are the hard stop on
cost, since one person can spread requests across addresses; the hourly
per-visitor caps stop one person using up everyone's share.
-}
data Limits = Limits
    { searchesToday :: Counter
    , photosToday :: Counter
    , searchesPerVisitor :: Counter
    , photosPerVisitor :: Counter
    , -- | Joins, vibe changes and likes, which all add to the database
      writesPerVisitor :: Counter
    , -- | Places our searches returned lately; photos are only served for these
      recentPlaces :: Recent
    }

-- | Read from TENDER_MAX_SEARCHES_PER_DAY and TENDER_MAX_PHOTOS_PER_DAY, or defaults
newLimits :: IO Limits
newLimits = do
    searches <- numberSetting "TENDER_MAX_SEARCHES_PER_DAY" 300
    photos <- numberSetting "TENDER_MAX_PHOTOS_PER_DAY" 3000
    newLimitsWith searches photos

-- | Limits with the given site-wide daily caps on searches and photos
newLimitsWith :: Int -> Int -> IO Limits
newLimitsWith searches photos = do
    let day = 24 * 60 * 60
        hour = 60 * 60
    Limits
        <$> newCounter (Limit day searches)
        <*> newCounter (Limit day photos)
        <*> newCounter (Limit hour 30)
        <*> newCounter (Limit hour 400)
        <*> newCounter (Limit hour 600)
        -- Long enough for a session, and place ids may be kept
        <*> newRecent (6 * hour)

{- | Spend from a visitor's and the site's allowance. Says why not when
either has run out.
-}
allowed :: Counter -> Counter -> Text -> Int -> IO (Maybe Text)
allowed perVisitor site visitor cost = do
    mine <- trySpend perVisitor visitor cost
    if not mine
        then pure (Just "You've hit Tender's limit for now. Try again in an hour.")
        else do
            ours <- trySpend site "site" cost
            pure $
                if ours
                    then Nothing
                    else Just "Tender has hit its daily limit. Try again tomorrow."

-- | Refuse once the visitor has changed too much this hour
spendWrite :: Env -> ActionM ()
spendWrite env = do
    visitor <- visitorOf
    ok <- liftIO $ trySpend (writesPerVisitor (envLimits env)) visitor 1
    unless ok $ errorResponse status429 "You've hit Tender's limit for now. Try again in an hour." >> finish

{- | Who is asking, for per-visitor limits. Cloud Run's front end appends the
address it got the request from to X-Forwarded-For, so the last entry is
the real one; earlier entries can be anything the visitor sent.
-}
visitorOf :: ActionM Text
visitorOf = do
    forwarded <- header "X-Forwarded-For"
    case reverse (map T.strip (T.splitOn "," (maybe "" TL.toStrict forwarded))) of
        (lastHop : _) | not (T.null lastHop) -> pure lastHop
        -- Running locally: the connection's address, without its port
        _ -> T.dropWhileEnd (/= ':') . T.pack . show . remoteHost <$> request

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
    port <- do
        tenderPort <- getEnv "TENDER_PORT"
        numberSetting (maybe "PORT" (const "TENDER_PORT") tenderPort) 5001
    static <- getEnv "TENDER_STATIC"
    initDb dbPath
    pruneHourly dbPath
    limits <- newLimits
    scotty port $ do
        middleware logStdout
        app (Env dbPath providers placesKey static limits)

{- | Delete sessions older than a day, every hour, so the database (in
memory on Cloud Run) doesn't grow for as long as the server runs
-}
pruneHourly :: FilePath -> IO ()
pruneHourly dbPath = void . forkIO . forever $ do
    threadDelay (60 * 60 * 1000000)
    result <- try (withDb dbPath (`pruneOlderThan` (24 * 60 * 60)))
    either (\e -> logMsg ("Pruning failed: " ++ show (e :: SomeException))) pure result

{- | A number from the environment, or the default when it isn't set.
Anything else stops the server, saying which setting is wrong.
-}
numberSetting :: String -> Int -> IO Int
numberSetting name def = do
    value <- getEnv name
    case value of
        Nothing -> pure def
        Just v -> maybe (die (name ++ " should be a whole number, not " ++ show v)) pure (readMaybe v)

logMsg :: MonadIO m => String -> m ()
logMsg = liftIO . hPutStrLn stderr

-- | Respond with an error status and a message the frontend can show
errorResponse :: Status -> Text -> ActionM ()
errorResponse s msg = status s >> json (object ["error" .= msg])


-- | Restaurants someone else in the session liked first, otherwise in the source's order
likedFirst :: [Text] -> [Restaurant] -> [Restaurant]
likedFirst ids rs = uncurry (<>) (partition ((`elem` ids) . R.restaurantId) rs)

{- | Sent with every response. Scripts, fonts and requests only come from
this site. Images can also be data urls (the QR code) and Google's and
Yelp's image servers, which photos redirect to. Styles allow inline
attributes, used for the confetti and the swipe stamps. The app can't be
put in a frame on another site.
-}
securityHeaders :: [(BS.ByteString, BS.ByteString)]
securityHeaders =
    [
        ( "Content-Security-Policy"
        , BS.intercalate
            "; "
            [ "default-src 'self'"
            , "script-src 'self'"
            , "style-src 'self' 'unsafe-inline'"
            , "img-src 'self' data: https://*.googleusercontent.com https://*.yelpcdn.com"
            , "font-src 'self'"
            , "connect-src 'self'"
            , "object-src 'none'"
            , "base-uri 'self'"
            , "form-action 'self'"
            , "frame-ancestors 'none'"
            ]
        )
    , ("X-Content-Type-Options", "nosniff")
    , ("X-Frame-Options", "DENY")
    , ("Referrer-Policy", "strict-origin-when-cross-origin")
    , ("Permissions-Policy", "geolocation=(self), camera=(), microphone=()")
    , ("Strict-Transport-Security", "max-age=31536000")
    ]

-- | Largest request body accepted, in bytes
maxBody :: Word64
maxBody = 16 * 1024

-- | Longest restaurant id accepted. Google's place ids are under 100 characters.
maxIdLength :: Int
maxIdLength = 200

-- | Width to ask Google for. Cards are at most about 450 css pixels wide.
photoWidth :: Int
photoWidth = 1200

-- | The app as a WAI application, for testing without a server
application :: Env -> IO Application
application = scottyApp . app

app :: Env -> ScottyM ()
app env = do
    let withConn :: (Connection -> IO a) -> IO a
        withConn = withDb (envDbPath env)
    middleware (addHeaders securityHeaders)
    -- Every request body is a few ids at most
    middleware (requestSizeLimitMiddleware (setMaxLengthForRequest (\_ -> pure (Just maxBody)) defaultRequestSizeLimitSettings))
    -- The limit is hit while a handler reads the body, so it's caught here
    defaultHandler $ Handler $ \(_ :: RequestSizeException) ->
        errorResponse status413 "Request too large"

    get "/api/newsession" $
        liftIO U.newSessionId >>= json

    get "/api/addtosession/:session" $ do
        sessionText <- pathParam "session"
        case U.sessionFromText sessionText of
            Nothing -> errorResponse status400 "Invalid session"
            Just s -> do
                -- Set when this browser has been in the session before
                returning <- (>>= U.userFromText) <$> queryParamMaybe "user"
                spendWrite env
                joined <- liftIO $ withConn $ \c -> joinSession c s returning
                maybe (errorResponse status409 "This session is full") json joined

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
            (Just s, Just chips) -> spendWrite env >> liftIO (withConn $ \c -> setVibe c s chips) >>= json . vibeJSON

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
                visitor <- visitorOf
                -- Searching further makes six Google calls
                let cost = if inner > 0 then 6 else 1
                    limits = envLimits env
                refused <- liftIO $ allowed (searchesPerVisitor limits) (searchesToday limits) visitor cost
                found <- case refused of
                    Just why -> errorResponse status429 why >> finish
                    Nothing -> liftIO $ searchWithFallback logMsg (envProviders env) (Search l chips radius inner)
                case found of
                    Nothing -> errorResponse status502 "Couldn't get restaurants from any source"
                    Just rs -> do
                        liftIO $ remember (recentPlaces (envLimits env)) (map R.restaurantId rs)
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
                -- Names look like places/<place>/photos/<photo>
                let place = T.takeWhile (/= '/') (T.drop (T.length "places/") photo)
                known <- liftIO $ seenRecently (recentPlaces (envLimits env)) place
                unless known $ errorResponse status404 "Not a place we showed" >> finish
                visitor <- visitorOf
                let limits = envLimits env
                refused <- liftIO $ allowed (photosPerVisitor limits) (photosToday limits) visitor 1
                maybe (pure ()) (\why -> errorResponse status429 why >> finish) refused
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
            Just _ | T.length r > maxIdLength -> errorResponse status400 "Invalid restaurant"
            Just u -> do
                spendWrite env
                (matches, others) <- liftIO $ withConn $ \c -> do
                    recordDecision c (Decision (U.userId u) r l)
                    (,) <$> matchesFor c u <*> othersLiked c u
                json (object ["matches" .= matches, "othersLiked" .= others])

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
    -- notFound starts out as 404. Warp happens to send files as 200 anyway,
    -- but nothing else does.
    status status200
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
