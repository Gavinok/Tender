{-# LANGUAGE ScopedTypeVariables #-}

module Yelp (YelpAPIKey (..), YelpError (..), reqRes, reqIMG) where

import Restaurant

import Control.Exception (try)
import Data.Aeson (eitherDecode, withObject, (.:))
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple (
    HttpException,
    Request,
    getResponseBody,
    getResponseStatusCode,
    httpLBS,
    parseRequest_,
    setRequestBodyLBS,
    setRequestHeaders,
    setRequestMethod,
    setRequestPath,
 )

newtype YelpAPIKey = YelpAPIKey String
fromKey :: YelpAPIKey -> String
fromKey (YelpAPIKey s) = s

-- TODO store restaurant's location and use that to pin point restaurants
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
            setRequestHeaders
                [ ("Authorization", auth)
                , ("Content-Type", "application/graphql")
                ]
                (parseRequest_ "https://api.yelp.com")

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

data YelpError
    = -- | The request never got a response
      YelpUnreachable HttpException
    | -- | Yelp answered with a non 200 status and this body
      YelpStatus Int L8.ByteString
    | -- | The response body was not what we expected
      YelpParseError String
    deriving (Show)

{- | Send the request and decode a 200 response with the given parser.
Network failures are returned rather than thrown.
-}
yelpRequest :: (AT.Value -> AT.Parser a) -> Request -> IO (Either YelpError a)
yelpRequest parser req = do
    result <- try (httpLBS req)
    pure $ case result of
        Left (e :: HttpException) -> Left (YelpUnreachable e)
        Right resp -> case getResponseStatusCode resp of
            200 -> either (Left . YelpParseError) Right $ do
                v <- eitherDecode (getResponseBody resp)
                AT.parseEither parser v
            code -> Left (YelpStatus code (getResponseBody resp))

reqRes :: YelpAPIKey -> Loc -> IO (Either YelpError Series)
reqRes key l = yelpRequest AT.parseJSON (lookupRes l (defYelpRequest key))

reqIMG :: YelpAPIKey -> String -> IO (Either YelpError String)
reqIMG key id_ =
    yelpRequest
        (withObject "Business" (.: "image_url"))
        (lookupIMG id_ (defYelpRequest key))
