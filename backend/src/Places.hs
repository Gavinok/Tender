{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Restaurants and photos from the Google Places API (New)
module Places (
    PlacesKey (..),
    googlePlaces,
    searchNearby,
    searchText,
    photoUri,
    parsePlaces,
    isPhotoName,
    searchRadius,
) where

import Data.Aeson (Value, object, withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Fetch (FetchError, fetchJSON)
import Network.HTTP.Simple (
    Header,
    Request,
    parseRequest_,
    setRequestBodyJSON,
    setRequestHeaders,
    setRequestPath,
    setRequestQueryString,
 )
import Provider (Provider (..))
import Vibe (Chip, Search (..), priceLevels, queryTerms, wantsOpenNow)
import Restaurant (Author (..), Loc (..), Photo (..), PhotoSource (..), Restaurant (..), Source (..))

newtype PlacesKey = PlacesKey BS.ByteString

-- | The most popular places nearby, or a text search when the group picked a vibe
googlePlaces :: PlacesKey -> Provider
googlePlaces key = Provider "Google Places" $ \(Search l chips) ->
    if null chips then searchNearby key l else searchText key l chips

-- | Photos kept per place, of the up to 10 Google returns
maxPhotos :: Int
maxPhotos = 5

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
searchNearby :: PlacesKey -> Loc -> IO (Either FetchError [Restaurant])
searchNearby key l =
    fetchJSON parsePlaces $
        withKey key [("X-Goog-FieldMask", fieldMask)] $
            setRequestBodyJSON body $
                parseRequest_ "POST https://places.googleapis.com/v1/places:searchNearby"
  where
    body =
        object
            [ "includedTypes" .= ["restaurant" :: Text]
            , -- Hotels count as restaurants when they have one
              "excludedPrimaryTypes" .= ["hotel", "lodging", "motel", "bed_and_breakfast" :: Text]
            , "maxResultCount" .= (20 :: Int)
            , "locationRestriction" .= object ["circle" .= object ["center" .= l, "radius" .= searchRadius]]
            ]

-- | Up to 20 places matching the vibe, preferring ones within 5km
searchText :: PlacesKey -> Loc -> [Chip] -> IO (Either FetchError [Restaurant])
searchText key l chips =
    fetchJSON parsePlaces $
        withKey key [("X-Goog-FieldMask", fieldMask)] $
            setRequestBodyJSON body $
                parseRequest_ "POST https://places.googleapis.com/v1/places:searchText"
  where
    body =
        object $
            [ "textQuery" .= queryTerms chips
            , "pageSize" .= (20 :: Int)
            , -- Text search can only be restricted to a rectangle: the box around the circle
              "locationRestriction" .= object ["rectangle" .= boxAround l searchRadius]
            , "openNow" .= wantsOpenNow chips
            ]
                <> ["priceLevels" .= levels | let levels = priceLevels chips, not (null levels)]

-- | The rectangle around a circle of the given radius in metres
boxAround :: Loc -> Double -> Value
boxAround (Loc lat lon) radius =
    object
        [ "low" .= Loc (lat - dLat) (lon - dLon)
        , "high" .= Loc (lat + dLat) (lon + dLon)
        ]
  where
    dLat = radius / 111320
    dLon = radius / (111320 * cos (lat * pi / 180))

-- | A short lived url for the photo, at most the given width
photoUri :: PlacesKey -> Text -> Int -> IO (Either FetchError Text)
photoUri key resource width =
    fetchJSON (withObject "Photo" (.: "photoUri")) $
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
        found <- p .:? "photos" .!= []
        Restaurant Google
            <$> p .: "id"
            <*> (p .: "displayName" >>= (.: "text"))
            <*> p .:? "googleMapsUri"
            <*> p .:? "rating"
            <*> (maybe Nothing priceSymbol <$> p .:? "priceLevel")
            <*> (p .:? "primaryTypeDisplayName" >>= traverse (.: "text"))
            -- Each photo shown is a billed request, so only the first few
            <*> traverse parsePhoto (take maxPhotos found)
            <*> p .: "location"
    parsePhoto = withObject "Photo" $ \ph -> do
        authors <- ph .:? "authorAttributions" .!= []
        Photo <$> (GooglePhoto <$> ph .: "name") <*> traverse parseAuthor (listToMaybe authors)
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
