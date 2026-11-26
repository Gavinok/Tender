-- | Calling the restaurant sources' JSON APIs
module Fetch (FetchError (..), fetchJSON) where

import Control.Exception (try)
import Data.Aeson (Value, eitherDecode)
import Data.Aeson.Types qualified as AT
import Data.ByteString.Lazy.Char8 qualified as L8
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Simple (HttpException, Request, getResponseBody, getResponseStatusCode, httpLBS, setRequestResponseTimeout)

data FetchError
    = -- | The request never got a response
      Unreachable HttpException
    | -- | A non 200 status and the body that came with it
      BadStatus Int L8.ByteString
    | -- | The body was not what we expected
      ParseError String
    deriving stock (Show)

{- | Send the request and decode a 200 response, giving up after the given
number of seconds. Network failures and timeouts are returned rather than
thrown, so a slow source fails over to the next one instead of holding up
the search for http-client's default 30 seconds.
-}
fetchJSON :: Int -> (Value -> AT.Parser a) -> Request -> IO (Either FetchError a)
fetchJSON seconds parser req = do
    result <- try (httpLBS (setRequestResponseTimeout (responseTimeoutMicro (seconds * 1000000)) req))
    pure $ case result of
        Left (e :: HttpException) -> Left (Unreachable e)
        Right resp -> case getResponseStatusCode resp of
            200 -> either (Left . ParseError) Right $ do
                v <- eitherDecode (getResponseBody resp)
                AT.parseEither parser v
            code -> Left (BadStatus code (getResponseBody resp))
