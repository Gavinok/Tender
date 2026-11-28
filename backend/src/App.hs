module App (
    server,
    application,
    Env (..),
    Limits,
    newLimitsWith,
    visitorLimitReached,
    siteLimitReached,
    sessionFull,
    noLocationYet,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, link)
import Control.Exception (SomeException, try)
import Control.Monad (forever, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Word (Word64)
import Database.SQLite.Simple (Connection, withImmediateTransaction)
import System.Environment.Blank (getEnv)
import Web.Scotty (
    ActionM,
    Handler (..),
    ScottyM,
    defaultHandler,
    file,
    finish,
    get,
    header,
    json,
    jsonData,
    middleware,
    notFound,
    pathParam,
    post,
    queryParam,
    queryParamMaybe,
    redirect,
    request,
    scotty,
    scottyApp,
    setHeader,
    status,
 )

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Network.HTTP.Types (Status, status200, status400, status404, status409, status413, status429, status502, status503)
import Network.Mime (MimeMap, defaultMimeMap, defaultMimeType, mimeByExt)
import Network.Wai (Application, rawPathInfo, remoteHost)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.RequestSizeLimit (defaultRequestSizeLimitSettings, requestSizeLimitMiddleware, setMaxLengthForRequest)
import Network.Wai.Request (RequestSizeException)
import System.Directory (doesFileExist)
import System.Exit (die)
import System.FilePath (takeExtension, (</>))
import Text.Read (readMaybe)

import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Db
import Fetch (FetchError)
import Limits (Counter, Limit (..), Recent, newCounter, newRecent, remember, seenRecently, trySpend)
import Log (LogFormat (..), Logger, Severity (..), newLogger)
import OpenStreetMap (openStreetMap)
import Places
import Presence (Presence, dealtNow, lookupAll, newPresence, nextHeartbeatFor, people, pruneStale, seenNow)
import Provider (Provider (..), searchWithFallback)
import Restaurant (Restaurant, RestaurantId (..))
import Restaurant qualified as R
import User qualified as U
import Vibe (Found, Search (..), chipFromId, maxRadius)
import Wire
import Yelp (YelpKey (..), yelp)

-- | Configuration shared by every request
data Env = Env
    { envDb :: DbPool
    , envProviders :: [Provider]
    -- ^ Tried in order until one finds restaurants
    , envPlacesKey :: Maybe PlacesKey
    -- ^ Needed to show Google's photos
    , envStatic :: Maybe FilePath
    -- ^ The built frontend to serve, when set (in production)
    , envLimits :: Limits
    , envLog :: Logger
    , envPresence :: Presence
    -- ^ Who in each session was heard from lately, and what they were shown
    , envFindPlaces :: Maybe (Text -> IO (Either FetchError [Found]))
    -- ^ Looks up where the group says they're eating; needs a Google key
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
    , writesPerVisitor :: Counter
    -- ^ Joins, vibe changes and likes, which all add to the database
    , recentPlaces :: Recent
    -- ^ Places our searches returned lately; photos are only served for these
    , heartbeatsPerUser :: Counter
    -- ^ Presence heartbeats, by user id: people on the same wifi share an address
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
        -- One every 10 seconds is expected; this allows bursts
        <*> newCounter (Limit 60 30)

{- | Spend from a visitor's and the site's allowance. Says why not when
either has run out.
-}
allowed :: Counter -> Counter -> Text -> Int -> IO (Maybe Text)
allowed perVisitor site visitor cost = do
    mine <- trySpend perVisitor visitor cost
    if not mine
        then pure (Just visitorLimitReached)
        else do
            ours <- trySpend site "site" cost
            pure $
                if ours
                    then Nothing
                    else Just siteLimitReached

-- | Refuse once the visitor has changed too much this hour
spendWrite :: Env -> ActionM ()
spendWrite env = do
    visitor <- visitorOf
    ok <- liftIO $ trySpend (writesPerVisitor (envLimits env)) visitor 1
    unless ok $ errorResponse status429 visitorLimitReached >> finish

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

-- | The lobby's shared choices for the session
lobbyState :: Connection -> U.SessionId -> IO Lobby
lobbyState c s = (\(chips, locked) place -> Lobby chips locked place) <$> getVibe c s <*> getPlace c s

{- | Restaurant sources in the order given by TENDER_SOURCES (default
google,yelp,openstreetmap), leaving out the ones without a key
-}
providersFromEnv :: Logger -> Maybe PlacesKey -> Maybe YelpKey -> String -> IO [Provider]
providersFromEnv logAt placesKey yelpKey order = concat <$> traverse provider (T.splitOn "," (T.pack order))
  where
    provider name = case (T.strip name, placesKey, yelpKey) of
        ("google", Just k, _) -> pure [googlePlaces k]
        ("google", Nothing, _) -> skip "Google Places" "GOOGLE_PLACES_KEY"
        ("yelp", _, Just k) -> pure [yelp k]
        ("yelp", _, Nothing) -> skip "Yelp" "YELP_KEY"
        ("openstreetmap", _, _) -> pure [openStreetMap]
        (other, _, _) -> fail ("Unknown source in TENDER_SOURCES: " ++ T.unpack other)
    skip label var = do
        logAt Info $ "Not using " ++ label ++ " since " ++ var ++ " is not set"
        pure []

server :: IO ()
server = do
    format <- getEnv "TENDER_LOG_FORMAT"
    logAt <- case format of
        Nothing -> pure (newLogger Plain)
        Just "plain" -> pure (newLogger Plain)
        Just "json" -> pure (newLogger Json)
        Just other -> die ("TENDER_LOG_FORMAT should be plain or json, not " ++ show other)
    -- Keys pasted into a secret or env var often end with a newline
    let key = fmap (TE.encodeUtf8 . T.strip . T.pack)
    placesKey <- fmap PlacesKey . key <$> getEnv "GOOGLE_PLACES_KEY"
    yelpKey <- fmap YelpKey . key <$> getEnv "YELP_KEY"
    order <- fromMaybe "google,yelp,openstreetmap" <$> getEnv "TENDER_SOURCES"
    providers <- providersFromEnv logAt placesKey yelpKey order
    logAt Info $ "Restaurant sources: " ++ T.unpack (T.intercalate ", " (map providerName providers))
    dbPath <- fromMaybe "test.db" <$> getEnv "TENDER_DB"
    -- Cloud Run tells the server its port through PORT
    port <- do
        tenderPort <- getEnv "TENDER_PORT"
        numberSetting (maybe "PORT" (const "TENDER_PORT") tenderPort) 5001
    static <- getEnv "TENDER_STATIC"
    initDb dbPath
    pool <- openPool dbPath
    presence <- newPresence
    pruneHourly logAt pool presence
    limits <- newLimits
    scotty port $ do
        -- Cloud Run logs every request itself, so only log them here locally
        unless (format == Just "json") $ middleware logStdout
        app (Env pool providers placesKey static limits logAt presence (findPlaces <$> placesKey))

{- | Delete sessions older than a day, every hour, so the database (in
memory on Cloud Run) doesn't grow for as long as the server runs. Should
the thread die anyway, the server goes down with it rather than carrying
on without pruning.
-}
pruneHourly :: Logger -> DbPool -> Presence -> IO ()
pruneHourly logAt pool presence = link =<< async (forever prune)
  where
    prune = do
        threadDelay (60 * 60 * 1000000)
        pruneStale (60 * 60) presence
        result <- try (withConn pool (`pruneOlderThan` (24 * 60 * 60)))
        either (\e -> logAt Error ("Pruning failed: " ++ show (e :: SomeException))) pure result

{- | A number from the environment, or the default when it isn't set.
Anything else stops the server, saying which setting is wrong.
-}
numberSetting :: String -> Int -> IO Int
numberSetting name def = do
    value <- getEnv name
    case value of
        Nothing -> pure def
        Just v -> maybe (die (name ++ " should be a whole number, not " ++ show v)) pure (readMaybe v)

-- | Messages the frontend shows as they are, named so the tests check the same words
visitorLimitReached, siteLimitReached, sessionFull, noLocationYet :: Text
visitorLimitReached = "You've hit Tender's limit for now. Try again in an hour."
siteLimitReached = "Tender has hit its daily limit. Try again tomorrow."
sessionFull = "This session is full"
noLocationYet = "Nobody else in this session has shared their location yet."

-- | Respond with an error status and a message the frontend can show
errorResponse :: Status -> Text -> ActionM ()
errorResponse s msg = status s >> json (ErrorResponse msg)

-- | Restaurants someone else in the session liked first, otherwise in the source's order
likedFirst :: Set.Set RestaurantId -> [Restaurant] -> [Restaurant]
likedFirst ids = uncurry (<>) . partition ((`Set.member` ids) . R.restaurantId)

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
    let db :: (Connection -> IO a) -> IO a
        db = withConn (envDb env)
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
                joined <- liftIO $ db $ \c -> joinSession c s returning
                maybe (errorResponse status409 sessionFull) json joined

    {- Heartbeat from someone in a session, every 10 seconds. Answers with
    everyone in it, whether they're swiping, done or away, and the user's
    matches, which the out-of-cards screen celebrates.
    -}
    post "/api/presence" $ do
        PresenceRequest uid <- jsonData
        known <- liftIO $ db (`getUser` uid)
        case known of
            Nothing -> errorResponse status404 "Unknown user"
            Just u -> do
                ok <- liftIO $ trySpend (heartbeatsPerUser (envLimits env)) (U.userIdText uid) 1
                unless ok $ errorResponse status429 "Too many heartbeats" >> finish
                liftIO $ seenNow (envPresence env) uid
                (members, matches) <- liftIO $ db $ \c -> (,) <$> sessionMembers c (U.session u) <*> matchesFor c u
                (now, seenBy) <- liftIO $ lookupAll (envPresence env)
                let everyone = people now uid members seenBy
                json (PresenceResponse everyone matches (nextHeartbeatFor everyone))

    -- The session's vibe chips, and whether swiping has started and locked them
    get "/api/vibe/:session" $ do
        sessionText <- pathParam "session"
        case U.sessionFromText sessionText of
            Nothing -> errorResponse status400 "Invalid session"
            Just s -> liftIO (db (`lobbyState` s)) >>= json

    post "/api/vibe/:session" $ do
        sessionText <- pathParam "session"
        VibeRequest ids <- jsonData
        case (U.sessionFromText sessionText, traverse chipFromId ids) of
            (Nothing, _) -> errorResponse status400 "Invalid session"
            (_, Nothing) -> errorResponse status400 "Unknown vibe"
            (Just s, Just chips) -> spendWrite env >> liftIO (db $ \c -> setVibe c s chips >> lobbyState c s) >>= json

    -- Where the group is eating, shared and locked like the vibe
    post "/api/place/:session" $ do
        sessionText <- pathParam "session"
        PlaceRequest place <- jsonData
        case U.sessionFromText sessionText of
            Nothing -> errorResponse status400 "Invalid session"
            Just s -> spendWrite env >> liftIO (db $ \c -> setPlace c s place >> lobbyState c s) >>= json

    -- Places matching what someone typed for where they're eating. Each
    -- lookup is a Google call, so it counts like a search.
    get "/api/places" $ do
        q <- T.strip <$> queryParam "q"
        case envFindPlaces env of
            Nothing -> errorResponse status503 "Looking up places needs GOOGLE_PLACES_KEY"
            Just find
                | T.length q < 2 || T.length q > 100 -> errorResponse status400 "Type between 2 and 100 characters"
                | otherwise -> do
                    visitor <- visitorOf
                    let limits = envLimits env
                    refused <- liftIO $ allowed (searchesPerVisitor limits) (searchesToday limits) visitor 1
                    maybe (pure ()) (\why -> errorResponse status429 why >> finish) refused
                    found <- liftIO (find q)
                    case found of
                        Right places -> json places
                        Left e -> do
                            liftIO $ envLog env Warning ("Looking up " ++ show q ++ " failed: " ++ show e)
                            errorResponse status502 "Couldn't look that up. Try again."

    -- Restaurants near the session's location, fresh for each person
    post "/api/restaurants" $ do
        RestaurantsRequest uid here wider <- jsonData
        known <- liftIO $ db $ \c -> getUser c uid
        case known of
            Nothing -> errorResponse status404 "Unknown user"
            Just u -> do
                -- One transaction, so these all come from the same moment. It writes
                -- (the location, a wider radius), so it takes the write lock up front.
                (located, (chips, _), radius, decided) <- liftIO $ db $ \c -> withImmediateTransaction c $ do
                    let s = U.session u
                    l <- sessionLocation c s here
                    radius <- maybe (sessionRadius c s) (widenSearch c s) wider
                    (l,,radius,) <$> getVibe c s <*> decidedBy c uid
                l <- maybe (errorResponse status409 noLocationYet >> finish) pure located
                -- Searching further only needs the ring outside what the person saw
                let inner = maybe 0 (min radius) wider
                visitor <- visitorOf
                -- Searching further makes six Google calls
                let cost = if inner > 0 then 6 else 1
                    limits = envLimits env
                refused <- liftIO $ allowed (searchesPerVisitor limits) (searchesToday limits) visitor cost
                found <- case refused of
                    Just why -> errorResponse status429 why >> finish
                    Nothing -> liftIO $ searchWithFallback (envLog env Warning) (envProviders env) (Search l chips radius inner)
                case found of
                    Nothing -> errorResponse status502 "Couldn't get restaurants from any source"
                    Just rs -> do
                        liftIO $ remember (recentPlaces (envLimits env)) (map R.restaurantId rs)
                        others <- liftIO $ db $ \c -> othersLiked c u
                        -- Leaving out ones this person already decided on, e.g. before a reload
                        let shown = likedFirst (Set.fromList others) (filter ((`Set.notMember` Set.fromList decided) . R.restaurantId) rs)
                        liftIO $ dealtNow (envPresence env) uid (map R.restaurantId shown)
                        json (RestaurantsResponse shown others chips radius maxRadius)

    -- Redirects to the photo with the given resource name, so the key stays here
    get "/api/photo" $ do
        photo <- queryParam "name"
        case envPlacesKey env of
            Nothing -> errorResponse status404 "Google photos need GOOGLE_PLACES_KEY"
            Just key
                | not (isPhotoName photo) -> errorResponse status400 "Not a photo name"
                | otherwise -> do
                    -- Names look like places/<place>/photos/<photo>
                    let place = RestaurantId $ T.takeWhile (/= '/') (T.drop (T.length "places/") photo)
                    known <- liftIO $ seenRecently (recentPlaces (envLimits env)) place
                    unless known $ errorResponse status404 "Not a place we showed" >> finish
                    visitor <- visitorOf
                    let limits = envLimits env
                    refused <- liftIO $ allowed (photosPerVisitor limits) (photosToday limits) visitor 1
                    maybe (pure ()) (\why -> errorResponse status429 why >> finish) refused
                    found <- liftIO $ photoUri key photo photoWidth
                    case found of
                        Left e -> do
                            liftIO $ envLog env Warning $ "Photo " ++ T.unpack photo ++ " failed: " ++ show e
                            errorResponse status502 "Couldn't get the photo from Google"
                        Right uri -> do
                            -- Lets the browser reuse it for the rest of the session
                            setHeader "Cache-Control" "private, max-age=3600"
                            redirect (TL.fromStrict uri)

    post "/api/decision" $ do
        DecisionRequest uid r l <- jsonData
        found <- liftIO $ db $ \c -> getUser c uid
        case found of
            Nothing -> errorResponse status404 "Unknown user"
            Just _ | let RestaurantId t = r in T.length t > maxIdLength -> errorResponse status400 "Invalid restaurant"
            Just u -> do
                spendWrite env
                (matches, others, members) <- liftIO $ db $ \c -> do
                    seenNow (envPresence env) (U.userId u)
                    recordDecision c (Decision (U.userId u) r l)
                    (,,) <$> matchesFor c u <*> othersLiked c u <*> sessionMembers c (U.session u)
                (now, seenBy) <- liftIO $ lookupAll (envPresence env)
                json (DecisionResponse matches others (people now (U.userId u) members seenBy))

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
            setHeader "Cache-Control" (cacheControl candidate)
            file candidate
        else do
            setHeader "Content-Type" "text/html; charset=utf-8"
            setHeader "Cache-Control" "no-cache"
            file (dir </> "index.html")

{- | Parcel puts a hash in every file name but index.html and the web app
manifest, whose links have to stay put. Those are checked on every use; the
rest never change.
-}
cacheControl :: FilePath -> TL.Text
cacheControl candidate
    | takeExtension candidate == ".webmanifest" = "no-cache"
    | otherwise = "public, max-age=31536000, immutable"

-- | From the file's extension, by 'mimeTypes'
contentType :: FilePath -> TL.Text
contentType = TL.fromStrict . TE.decodeUtf8 . mimeByExt mimeTypes defaultMimeType . T.pack

-- | mime-types' table of the common ones, plus the web app manifest it lacks
mimeTypes :: MimeMap
mimeTypes = Map.insert "webmanifest" "application/manifest+json" defaultMimeMap
