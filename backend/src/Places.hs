{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Restaurants and photos from the Google Places API (New)
module Places (
    PlacesKey (..),
    PlacesError (..),
    searchNearby,
    photoUri,
    parsePlaces,
    isPhotoName,
    searchRadius,
) where

import Control.Exception (try)
import Data.Aeson (Value, eitherDecode, object, withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (isAlphaNum)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Simple (
    Header,
    HttpException,
    Request,
    getResponseBody,
    getResponseStatusCode,
    httpLBS,
    parseRequest_,
    setRequestBodyJSON,
    setRequestHeaders,
    setRequestPath,
    setRequestQueryString,
 )
import Restaurant (Author (..), Loc, Photo (..), Restaurant (..))

newtype PlacesKey = PlacesKey BS.ByteString

data PlacesError
    = -- | The request never got a response
      PlacesUnreachable HttpException
    | -- | Google answered with a non 200 status and this body
      PlacesStatus Int L8.ByteString
    | -- | The response body was not what we expected
      PlacesParseError String
    deriving (Show)

-- | How far from the user to search, in metres
searchRadius :: Double
searchRadius = 5000

{- | Fields shown on the card. Google bills a request at the most expensive
field asked for, and rating and priceLevel put this in the Enterprise SKU.
-}
fieldMask :: BS.ByteString
fieldMask = "places.id,places.displayName,places.location,places.rating,places.priceLevel,places.googleMapsUri,places.primaryTypeDisplayName,places.photos"

-- | The key goes in a header rather than the url so it can't end up in logs
withKey :: PlacesKey -> [Header] -> Request -> Request
withKey (PlacesKey k) headers = setRequestHeaders (("X-Goog-Api-Key", k) : headers)

-- | Up to 20 restaurants near the location, most popular first
searchNearby :: PlacesKey -> Loc -> IO (Either PlacesError [Restaurant])
searchNearby key l =
    send parsePlaces $
        withKey key [("X-Goog-FieldMask", fieldMask)] $
            setRequestBodyJSON body $
                parseRequest_ "POST https://places.googleapis.com/v1/places:searchNearby"
  where
    body =
        object
            [ "includedTypes" .= ["restaurant" :: Text]
            , "maxResultCount" .= (20 :: Int)
            , "locationRestriction" .= object ["circle" .= object ["center" .= l, "radius" .= searchRadius]]
            ]

-- | A short lived url for the photo, at most the given width
photoUri :: PlacesKey -> Text -> Int -> IO (Either PlacesError Text)
photoUri key resource width =
    send (withObject "Photo" (.: "photoUri")) $
        withKey key [] $
            setRequestQueryString [("maxWidthPx", Just (TE.encodeUtf8 (T.pack (show width)))), ("skipHttpRedirect", Just "true")] $
                setRequestPath (TE.encodeUtf8 ("/v1/" <> resource <> "/media")) $
                    parseRequest_ "GET https://places.googleapis.com"

{- | Whether the text has the shape of a photo's resource name. The photo
endpoint only passes these on, so it can't be used to call anything else.
-}
isPhotoName :: Text -> Bool
isPhotoName t = case T.splitOn "/" t of
    ["places", place, "photos", ref] -> all segment [place, ref]
    _ -> False
  where
    segment s = not (T.null s) && T.all (\c -> isAlphaNum c || c == '-' || c == '_') s

parsePlaces :: Value -> AT.Parser [Restaurant]
parsePlaces = withObject "SearchNearbyResponse" $ \o ->
    -- Google leaves the field out when nothing is found
    o .:? "places" .!= [] >>= traverse place
  where
    place = withObject "Place" $ \p -> do
        photos <- p .:? "photos" .!= []
        Restaurant
            <$> p .: "id"
            <*> (p .: "displayName" >>= (.: "text"))
            <*> p .:? "googleMapsUri"
            <*> p .:? "rating"
            <*> (maybe Nothing priceSymbol <$> p .:? "priceLevel")
            <*> (p .:? "primaryTypeDisplayName" >>= traverse (.: "text"))
            <*> traverse parsePhoto (listToMaybe photos)
            <*> p .: "location"
    parsePhoto = withObject "Photo" $ \ph -> do
        authors <- ph .:? "authorAttributions" .!= []
        Photo <$> ph .: "name" <*> traverse parseAuthor (listToMaybe authors)
    parseAuthor = withObject "AuthorAttribution" $ \a ->
        Author <$> a .: "displayName" <*> (fmap absolute <$> a .:? "uri")
    -- Author links come back without a scheme, e.g. //maps.google.com/...
    absolute u = if "//" `T.isPrefixOf` u then "https:" <> u else u

priceSymbol :: Text -> Maybe Text
priceSymbol = \case
    "PRICE_LEVEL_FREE" -> Just "Free"
    "PRICE_LEVEL_INEXPENSIVE" -> Just "$"
    "PRICE_LEVEL_MODERATE" -> Just "$$"
    "PRICE_LEVEL_EXPENSIVE" -> Just "$$$"
    "PRICE_LEVEL_VERY_EXPENSIVE" -> Just "$$$$"
    _ -> Nothing

-- | Send the request and decode a 200 response. Network failures are returned rather than thrown.
send :: (Value -> AT.Parser a) -> Request -> IO (Either PlacesError a)
send parser req = do
    result <- try (httpLBS req)
    pure $ case result of
        Left (e :: HttpException) -> Left (PlacesUnreachable e)
        Right resp -> case getResponseStatusCode resp of
            200 -> either (Left . PlacesParseError) Right $ do
                v <- eitherDecode (getResponseBody resp)
                AT.parseEither parser v
            code -> Left (PlacesStatus code (getResponseBody resp))
