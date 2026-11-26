-- | The handlers, run as a WAI application against a fake restaurant source
module Endpoints (spec) where

import Control.Monad (forM_, replicateM_)
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Char8 qualified as S8
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types (methodGet, methodPost, status413)
import Network.Wai (Application, RequestBodyLength (..), defaultRequest, requestBodyLength, requestMethod)
import Network.Wai.Test (SRequest (..), runSession, setPath, srequest)
import System.Directory (createDirectory, getTemporaryDirectory)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Hspec
import Test.Hspec.Wai

import App (Env (..), application, newLimitsWith)
import Db (initDb, openPool)
import Places (PlacesKey (..))
import Provider (Provider (..))
import Restaurant

-- | Always finds the same two places, without calling anyone
fake :: Provider
fake = Provider "Fake" (const (pure (Right [place "p1", place "p2"])))
  where
    place i = Restaurant Google (RestaurantId i) i Nothing Nothing Nothing Nothing [] Nothing (Loc 48.43 (-123.35))

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
appWith searchCap photoCap static = do
    db <- freshDb
    limits <- newLimitsWith searchCap photoCap
    pool <- openPool db
    application (Env pool [fake] (Just (PlacesKey "not a real key")) static limits (\_ _ -> pure ()))

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
                withoutLoc other `shouldRespondWith` "{\"error\":\"Nobody else in this session has shared their location yet.\"}"{matchStatus = 409}
                u <- unquote . simpleBody <$> get ("/api/addtosession/" <> encodeText s)
                search u `shouldRespondWith` 200
                withoutLoc other `shouldRespondWith` 200
            it "stops a visitor after 30 searches an hour" $ do
                (_, u) <- joined
                replicateM_ 30 (search u `shouldRespondWith` 200)
                search u `shouldRespondWith` "{\"error\":\"You've hit Tender's limit for now. Try again in an hour.\"}"{matchStatus = 429}

        describe "GET /api/addtosession" $ do
            it "turns away the 31st person" $ do
                s <- newSession
                let path = "/api/addtosession/" <> encodeText s
                replicateM_ 30 (get path `shouldRespondWith` 200)
                get path `shouldRespondWith` "{\"error\":\"This session is full\"}"{matchStatus = 409}

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
                decide a "p1" True `shouldRespondWith` "{\"matches\":[],\"othersLiked\":[]}"
                decide b "p1" True `shouldRespondWith` "{\"matches\":[\"p1\"],\"othersLiked\":[]}"
                decide a "p2" False `shouldRespondWith` "{\"matches\":[\"p1\"],\"othersLiked\":[]}"

        describe "GET /api/photo"
            $ it "only serves photos of places a search returned"
            $ get "/api/photo?name=places/elsewhere/photos/x" `shouldRespondWith` 404

    -- No photos left today, so a known place stops at the limit before calling Google
    with (appWith 1 0 Nothing) $ do
        it "stops the site at its daily search cap" $ do
            (_, u) <- joined
            search u `shouldRespondWith` 200
            search u `shouldRespondWith` "{\"error\":\"Tender has hit its daily limit. Try again tomorrow.\"}"{matchStatus = 429}
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
                get "/app.js" `shouldRespondWith` "console.log(1)"{matchHeaders = ["Content-Type" <:> "text/javascript"]}
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
        appWith 1000 1000 (Just (root </> "static"))
