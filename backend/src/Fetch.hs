{-# LANGUAGE ScopedTypeVariables #-}

-- | Calling the restaurant sources' JSON APIs
module Fetch (FetchError (..), fetchJSON) where

import Control.Exception (try)
import Data.Aeson (Value, eitherDecode)
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple (HttpException, Request, getResponseBody, getResponseStatusCode, httpLBS)

data FetchError
    = -- | The request never got a response
      Unreachable HttpException
    | -- | A non 200 status and the body that came with it
      BadStatus Int L8.ByteString
    | -- | The body was not what we expected
      ParseError String
    deriving (Show)

-- | Send the request and decode a 200 response. Network failures are returned rather than thrown.
fetchJSON :: (Value -> AT.Parser a) -> Request -> IO (Either FetchError a)
fetchJSON parser req = do
    result <- try (httpLBS req)
    pure $ case result of
        Left (e :: HttpException) -> Left (Unreachable e)
        Right resp -> case getResponseStatusCode resp of
            200 -> either (Left . ParseError) Right $ do
                v <- eitherDecode (getResponseBody resp)
                AT.parseEither parser v
            code -> Left (BadStatus code (getResponseBody resp))
