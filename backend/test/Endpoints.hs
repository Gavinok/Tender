-- | The handlers, run as a WAI application against a fake restaurant source
module Endpoints (spec) where

import Control.Monad (forM_, replicateM_)
import Data.Aeson (ToJSON, encode, object, (.=))
import Data.ByteString.Char8 qualified as S8
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types (methodGet, methodPost, status413)
import Network.Wai (Application, RequestBodyLength (..), defaultRequest, requestBodyLength, requestMethod)
import Network.Wai.Test (SRequest (..), runSession, setPath, srequest)
import System.Directory (createDirectory, getTemporaryDirectory)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher (MatchBody (..), bodyEquals)

import App (Env (..), application, newLimitsWith, noLocationYet, sessionFull, siteLimitReached, visitorLimitReached)
import Db (initDb, openPool)
import Fetch (FetchError)
import Fixtures
import Places (PlacesKey (..))
import Presence (Activity (..), Person (..), newPresence)
import Provider (Provider (..))
import Restaurant
import Vibe (Chip (..), Found (..), Place (..))
import Wire (DecisionResponse (..), ErrorResponse (..), Lobby (..), PresenceResponse (..))

-- | Always finds the same two places, without calling anyone
fake :: Provider
fake = Provider "Fake" (const (pure (Right [place "p1", place "p2"])))
  where
    place i = plain Google i (Loc 48.43 (-123.35))

-- | A fresh database file, so each test starts empty
freshDb :: IO FilePath
freshDb = do
    tmp <- getTemporaryDirectory
    (path, h) <- openTempFile tmp "tender-test.db"
    hClose h
    initDb path
    pure path

-- | The app with the given site-wide daily caps on searches and photos
appWith :: Int -> Int -> Maybe FilePath -> IO Application
appWith searchCap photoCap static = appLooking searchCap photoCap static (Just lookUp)

-- | A 200 with the JSON of the value, encoded as the handler encodes it
respondsWith :: (ToJSON a) => WaiSession st SResponse -> a -> WaiExpectation st
respondsWith action expected = action `shouldRespondWith` 200{matchBody = bodyEquals (encode expected)}

-- | An error status with the message the frontend shows
failsWith :: WaiSession st SResponse -> (Integer, Text) -> WaiExpectation st
failsWith action (code, message) =
    action `shouldRespondWith` (fromInteger code){matchBody = bodyEquals (encode (ErrorResponse message))}

p1 :: RestaurantId
p1 = RestaurantId "p1"

-- | Finds Kelowna for anything, without calling anyone
lookUp :: Text -> IO (Either FetchError [Found])
lookUp _ = pure (Right [Found kelowna "Kelowna, BC, Canada"])

kelowna :: Place
kelowna = Place "Kelowna" (Loc 49.88 (-119.49))

appLooking :: Int -> Int -> Maybe FilePath -> Maybe (Text -> IO (Either FetchError [Found])) -> IO Application
appLooking searchCap photoCap static lookup' = do
    db <- freshDb
    limits <- newLimitsWith searchCap photoCap
    pool <- openPool db
    presence <- newPresence
    application (Env pool [fake] (Just (PlacesKey "not a real key")) static limits (\_ _ -> pure ()) presence lookup')

app :: IO Application
app = appWith 1000 1000 Nothing

postJSON :: S8.ByteString -> L8.ByteString -> WaiSession st SResponse
postJSON path = request methodPost path [("Content-Type", "application/json")]

-- | Start a session and join it, giving the session and user ids
joined :: WaiSession st (Text, Text)
joined = do
    s <- newSession
    u <- unquote . simpleBody <$> get ("/api/addtosession/" <> encodeText s)
    pure (s, u)

newSession :: WaiSession st Text
newSession = unquote . simpleBody <$> get "/api/newsession"

-- | A JSON string's contents
unquote :: L8.ByteString -> Text
unquote = read . L8.unpack

encodeText :: Text -> S8.ByteString
encodeText = TE.encodeUtf8

search :: Text -> WaiSession st SResponse
search u = postJSON "/api/restaurants" (encode (object ["user" .= u, "loc" .= Loc 48.43 (-123.35)]))

spec :: Spec
spec = do
    with app $ do
        describe "POST /api/restaurants" $ do
            it "finds restaurants from the source" $ do
                (_, u) <- joined
                search u `shouldRespondWith` 200
            it "refuses a location off the globe" $ do
                (_, u) <- joined
                postJSON "/api/restaurants" "{\"user\":\"x\",\"loc\":{\"latitude\":500,\"longitude\":0}}"
                    `shouldRespondWith` 422
                search u `shouldRespondWith` 200
            it "uses the session's location for someone who has none" $ do
                (s, _) <- joined
                other <- unquote . simpleBody <$> get ("/api/addtosession/" <> encodeText s)
                let withoutLoc u = postJSON "/api/restaurants" (encode (object ["user" .= u]))
                withoutLoc other `failsWith` (409, noLocationYet)
                u <- unquote . simpleBody <$> get ("/api/addtosession/" <> encodeText s)
                search u `shouldRespondWith` 200
                withoutLoc other `shouldRespondWith` 200
            it "stops a visitor after 30 searches an hour" $ do
                (_, u) <- joined
                replicateM_ 30 (search u `shouldRespondWith` 200)
                search u `failsWith` (429, visitorLimitReached)

        describe "GET /api/addtosession" $ do
            it "turns away the 31st person" $ do
                s <- newSession
                let path = "/api/addtosession/" <> encodeText s
                replicateM_ 30 (get path `shouldRespondWith` 200)
                get path `failsWith` (409, sessionFull)

        describe "POST /api/decision" $ do
            it "refuses an overly long restaurant id" $ do
                (_, u) <- joined
                postJSON "/api/decision" (encode (object ["user" .= u, "restaurant" .= L8.unpack (L8.replicate 300 'x'), "liked" .= True]))
                    `shouldRespondWith` 400
            it "tells the first liker about a match on their next swipe" $ do
                s <- newSession
                let path = "/api/addtosession/" <> encodeText s
                a <- unquote . simpleBody <$> get path
                b <- unquote . simpleBody <$> get path
                let decide u r l = postJSON "/api/decision" (encode (object ["user" .= u, "restaurant" .= (r :: Text), "liked" .= l]))
                decide a "p1" True `respondsWith` DecisionResponse [] [] [Person True Swiping 1 Nothing, Person False Away 0 Nothing]
                decide b "p1" True `respondsWith` DecisionResponse [p1] [] [Person False Swiping 1 Nothing, Person True Swiping 1 Nothing]
                decide a "p2" False `respondsWith` DecisionResponse [p1] [] [Person True Swiping 2 Nothing, Person False Swiping 1 Nothing]

        describe "what the group is craving" $ do
            let crave s t = postJSON ("/api/craving/" <> encodeText s) (encode (object ["craving" .= (t :: Text)]))
            it "is shared in the lobby, tidied, and cleared when blank" $ do
                s <- newSession
                crave s "  spicy   ramen " `respondsWith` Lobby [] False Nothing (Just "spicy ramen")
                get ("/api/vibe/" <> encodeText s) `respondsWith` Lobby [] False Nothing (Just "spicy ramen")
                crave s " " `respondsWith` Lobby [] False Nothing Nothing
            it "leaves the chips alone, and the chips leave it alone" $ do
                s <- newSession
                _ <- postJSON ("/api/vibe/" <> encodeText s) "{\"chips\":[\"patio\"]}"
                crave s "tacos" `respondsWith` Lobby [Patio] False Nothing (Just "tacos")
                postJSON ("/api/vibe/" <> encodeText s) "{\"chips\":[]}" `respondsWith` Lobby [] False Nothing (Just "tacos")
            it "refuses a craving that's too long" $ do
                s <- newSession
                crave s (T.replicate 61 "a") `shouldRespondWith` 400
            it "goes with the restaurants, and locks once someone starts swiping" $ do
                (s, u) <- joined
                _ <- crave s "ramen"
                search u `shouldRespondWith` 200{matchBody = MatchBody (\_ body -> if "\"craving\":\"ramen\"" `S8.isInfixOf` L8.toStrict body then Nothing else Just "no craving in the response")}
                crave s "tacos" `respondsWith` Lobby [] True Nothing (Just "ramen")

        describe "where the group is eating" $ do
            let choose s place = postJSON ("/api/place/" <> encodeText s) (encode (object ["place" .= place]))
            it "is shared in the lobby and can be cleared" $ do
                s <- newSession
                get ("/api/vibe/" <> encodeText s) `respondsWith` Lobby [] False Nothing Nothing
                choose s (Just kelowna) `respondsWith` Lobby [] False (Just kelowna) Nothing
                get ("/api/vibe/" <> encodeText s) `respondsWith` Lobby [] False (Just kelowna) Nothing
                choose s (Nothing :: Maybe Place) `respondsWith` Lobby [] False Nothing Nothing
            it "is searched from without anyone's location" $ do
                (s, u) <- joined
                _ <- choose s (Just kelowna)
                postJSON "/api/restaurants" (encode (object ["user" .= u])) `shouldRespondWith` 200
            it "locks once someone starts swiping" $ do
                (s, u) <- joined
                _ <- search u
                choose s (Just kelowna) `respondsWith` Lobby [] True Nothing Nothing
            it "refuses a place off the globe or without a name" $ do
                s <- newSession
                postJSON ("/api/place/" <> encodeText s) "{\"place\":{\"name\":\"Nowhere\",\"latitude\":500,\"longitude\":0}}" `shouldRespondWith` 422
                postJSON ("/api/place/" <> encodeText s) "{\"place\":{\"name\":\" \",\"latitude\":1,\"longitude\":2}}" `shouldRespondWith` 422

        describe "GET /api/places" $ do
            it "lists matches for what was typed" $
                get "/api/places?q=kelowna" `respondsWith` [Found kelowna "Kelowna, BC, Canada"]
            it "wants between 2 and 100 characters" $ do
                get "/api/places?q=k" `shouldRespondWith` 400
                get ("/api/places?q=" <> S8.replicate 101 'k') `shouldRespondWith` 400

        describe "POST /api/presence" $ do
            let heartbeat u = postJSON "/api/presence" (encode (object ["user" .= u]))
            it "shows who is swiping and who hasn't searched yet, in joining order" $ do
                (s, a) <- joined
                b <- unquote . simpleBody <$> get ("/api/addtosession/" <> encodeText s)
                _ <- search a
                heartbeat a `respondsWith` PresenceResponse [Person True Swiping 0 (Just 2), Person False Away 0 Nothing] [] 10
                heartbeat b `respondsWith` PresenceResponse [Person False Swiping 0 (Just 2), Person True Swiping 0 Nothing] [] 10
            it "counts decisions and says when someone is done" $ do
                (_, a) <- joined
                _ <- search a
                let decide r = postJSON "/api/decision" (encode (object ["user" .= a, "restaurant" .= (r :: Text), "liked" .= False]))
                _ <- decide "p1"
                heartbeat a `respondsWith` PresenceResponse [Person True Swiping 1 (Just 1)] [] 10
                _ <- decide "p2"
                -- Done, so sooner: waiting to hear about matches
                heartbeat a `respondsWith` PresenceResponse [Person True Done 2 (Just 0)] [] 5
            it "brings matches others completed" $ do
                s <- newSession
                let path = "/api/addtosession/" <> encodeText s
                a <- unquote . simpleBody <$> get path
                b <- unquote . simpleBody <$> get path
                let decide u r = postJSON "/api/decision" (encode (object ["user" .= u, "restaurant" .= (r :: Text), "liked" .= True]))
                _ <- decide a "p1"
                _ <- decide b "p1"
                heartbeat a `respondsWith` PresenceResponse [Person True Swiping 1 Nothing, Person False Swiping 1 Nothing] [p1] 10
            it "doesn't know users who never joined" $
                heartbeat ("00000000-0000-0000-0000-000000000000" :: Text) `shouldRespondWith` 404
            it "limits heartbeats per user" $ do
                (_, a) <- joined
                replicateM_ 30 (heartbeat a `shouldRespondWith` 200)
                heartbeat a `shouldRespondWith` 429

        describe "GET /api/photo"
            $ it "only serves photos of places a search returned"
            $ get "/api/photo?name=places/elsewhere/photos/x" `shouldRespondWith` 404

    with (appWith 1 1000 Nothing) $
        it "counts place lookups against the daily search cap" $ do
            get "/api/places?q=kelowna" `shouldRespondWith` 200
            get "/api/places?q=kelowna" `shouldRespondWith` 429

    with (appLooking 1000 1000 Nothing Nothing)
        $ it "can't look up places without a Google key"
        $ get "/api/places?q=kelowna" `shouldRespondWith` 503

    -- No photos left today, so a known place stops at the limit before calling Google
    with (appWith 1 0 Nothing) $ do
        it "stops the site at its daily search cap" $ do
            (_, u) <- joined
            search u `shouldRespondWith` 200
            search u `failsWith` (429, siteLimitReached)
        it "lets photos of places a search returned through to the limits" $ do
            (_, u) <- joined
            _ <- search u
            get "/api/photo?name=places/p1/photos/x" `shouldRespondWith` 429

    -- hspec-wai always says a body is 0 bytes long, so these set the length themselves
    describe "request bodies" $
        forM_ [("of a known length", KnownLength 20000), ("sent in chunks", ChunkedBody)] $ \(how, len) ->
            it ("refuses one over 16 KB " <> how) $ do
                application' <- app
                let req = (setPath defaultRequest "/api/decision"){requestMethod = methodPost, requestBodyLength = len}
                resp <- runSession (srequest (SRequest req (L8.replicate 20000 'a'))) application'
                simpleStatus resp `shouldBe` status413

    with frontend $
        describe "the frontend" $ do
            it "serves its files" $
                get "/app.js" `shouldRespondWith` "console.log(1)"{matchHeaders = ["Content-Type" <:> "text/javascript", "Cache-Control" <:> "public, max-age=31536000, immutable"]}
            it "serves the web app manifest as one" $
                get "/manifest.webmanifest" `shouldRespondWith` "{}"{matchHeaders = ["Content-Type" <:> "application/manifest+json", "Cache-Control" <:> "no-cache"]}
            it "serves the app for a session link" $
                get "/0b3f" `shouldRespondWith` "<html></html>"
            it "never serves anything outside its directory" $
                forM_ ["/../secret.txt", "/./../secret.txt", "/a/../../secret.txt"] $ \path ->
                    request methodGet path [] "" `shouldRespondWith` "<html></html>"
  where
    frontend = do
        tmp <- getTemporaryDirectory
        (dir, h) <- openTempFile tmp "tender-static"
        hClose h
        let root = dir <> ".d"
        createDirectory root
        createDirectory (root </> "static")
        writeFile (root </> "secret.txt") "secret"
        writeFile (root </> "static" </> "index.html") "<html></html>"
        writeFile (root </> "static" </> "app.js") "console.log(1)"
        writeFile (root </> "static" </> "manifest.webmanifest") "{}"
        appWith 1000 1000 (Just (root </> "static"))
