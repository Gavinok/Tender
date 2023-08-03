module Yelp (YelpAPIKey (..), YelpRequestResult (..), reqRes, reqIMG) where

import Restaurant

import Data.Aeson (decode, (.:))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple (
    Request,
    Response,
    defaultRequest,
    getResponseBody,
    getResponseStatusCode,
    httpJSON,
    httpLBS,
    setRequestBodyLBS,
    setRequestHeaders,
    setRequestHost,
    setRequestMethod,
    setRequestPath,
 )
import System.Environment.Blank (getEnv)

newtype YelpAPIKey = YelpAPIKey String
fromKey :: YelpAPIKey -> String
fromKey (YelpAPIKey s) = s

-- TODO store resturant's location and use that to pin point resturants
-- curl -X POST -H "Authorization: Bearer $YELP_KEY" -H "Content-Type: application/graphql" https://api.yelp.com/v3/graphql --data graphQLQuery
graphQLQuery :: L8.ByteString -> L8.ByteString -> L8.ByteString
graphQLQuery lat long =
    "{\
    \search(term:\"restaurant\", \
    \latitude: "
        <> lat
        <> ",  \
           \longitude: "
        <> long
        <> ", \
            \ limit: "
        <> "50"
        <> ") \
           \{ \
           \total \
           \business { \
           \id \
           \url \
           \name \
           \rating \
           \categories { title } \
           \price \
           \hours{ \
           \is_open_now\
           \}\
           \}\
           \}\
           \}"

newtype PartialYelpRequest = PartialYelpRequest Request
fromPartialYelpReq :: PartialYelpRequest -> Request
fromPartialYelpReq (PartialYelpRequest req) = req

defYelpRequest :: YelpAPIKey -> PartialYelpRequest
defYelpRequest key =
    let auth :: BS.ByteString
        auth = S8.pack $ "Bearer " ++ fromKey key
     in PartialYelpRequest $
            setRequestHost "api.yelp.com" $
                setRequestHeaders
                    [ ("Authorization", auth)
                    , ("Content-Type", "application/graphql")
                    ]
                    defaultRequest

lookupRes :: Loc -> PartialYelpRequest -> Request
lookupRes (Loc lat long) baseReq =
    setRequestPath "/v3/graphql" $
        setRequestMethod "POST" $
            setRequestBodyLBS (graphQLQuery (L8.pack (show lat)) (L8.pack (show long))) $
                fromPartialYelpReq baseReq

lookupIMG :: String -> PartialYelpRequest -> Request
lookupIMG id_ baseReq =
    setRequestPath (S8.pack ("/v3/businesses/" ++ id_)) $
        setRequestMethod "GET" $
            fromPartialYelpReq baseReq

data YelpRequestResult contained x
    = Was contained
    | FailedAPIRequest (Response x)
    | FailedToParse
    deriving (Show)

reqRes :: YelpAPIKey -> Loc -> IO (YelpRequestResult Series (Maybe Series))
reqRes key loc = do
    let req = lookupRes loc (defYelpRequest key)
    print req
    resp <- httpJSON req :: IO (Response (Maybe Series))
    _ <- print resp
    pure $ case getResponseStatusCode resp of
        200 ->
            maybe
                FailedToParse
                Was
                $ getResponseBody resp
        _ -> FailedAPIRequest resp

reqIMG :: YelpAPIKey -> String -> IO (YelpRequestResult String L8.ByteString)
reqIMG key id_ = do
    let req = lookupIMG id_ (defYelpRequest key)
    print req
    resp <- httpLBS req
    pure $ case getResponseStatusCode resp of
        200 -> case decode (getResponseBody resp) of
            Just (A.Object o) ->
                maybe
                    FailedToParse
                    Was
                    (AT.parseMaybe (.: "image_url") o :: Maybe String)
            _ -> FailedToParse
        _ -> FailedAPIRequest resp

--- Tests
testSeriesReq :: IO ()
testSeriesReq = do
    keyMaybe <- getEnv "YELP_KEY"
    case keyMaybe of
        Nothing -> fail "Failed to find Yelp Key"
        Just k -> do
            res <- reqRes (YelpAPIKey k) (Loc 48 (-122))
            case res of
                Was r -> print r
                _ -> fail "failed request"

testIMGReq :: IO [Char]
testIMGReq = do
    keyMaybe <- getEnv "YELP_KEY"
    case keyMaybe of
        Nothing -> fail "Failed to find Yelp Key"
        Just k -> do
            res <- reqRes (YelpAPIKey k) (Loc 48 (-122))
            case res of
                Was (Series s) -> do
                    r <- reqIMG (YelpAPIKey k) $ bid $ head s
                    case r of
                        Was img -> pure img
                        _ -> fail "failed on image"
                _ -> fail "failed request"
