{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Restaurants and photos from the Google Places API (New)
module Places (
    PlacesKey (..),
    withKey,
    googlePlaces,
    searchNearby,
    searchText,
    ringCircles,
    inRing,
    metres,
    photoUri,
    parsePlaces,
    isPhotoName,
) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (Value, object, withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum)
import Data.Either (partitionEithers)
import Data.List (nubBy, transpose)
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
import Network.HTTP.Client (redactHeaders)
import qualified Data.Set as Set
import Provider (Provider (..))
import Vibe (Chip, Search (..), priceLevels, queryTerms, wantsOpenNow)
import Restaurant (Author (..), Loc (..), Photo (..), PhotoSource (..), Restaurant (..), Source (..), Summary (..))

newtype PlacesKey = PlacesKey BS.ByteString

-- | The most popular places nearby, or a text search when the group picked a vibe
googlePlaces :: PlacesKey -> Provider
googlePlaces key = Provider "Google Places" $ \(Search l chips radius inner) ->
    let search centre r = if null chips then searchNearby key centre r else searchText key centre chips r
     in if inner <= 0
            then search l radius
            else ringSearch l inner radius search

{- | Search the ring between two radii around a location. Searching the
whole wider circle would return mostly the same popular places as the
inner one, so instead search six circles spread around the ring, keep
what falls in it, and interleave them so the results cover every
direction. The six searches run at once.
-}
ringSearch :: Loc -> Double -> Double -> (Loc -> Double -> IO (Either FetchError [Restaurant])) -> IO (Either FetchError [Restaurant])
ringSearch l inner outer search = do
    found <- mapConcurrently (uncurry search) (ringCircles l inner outer)
    pure $ case partitionEithers found of
        (e : _, []) -> Left e
        (_, lists) -> Right (inRing l inner outer (interleave lists))

-- | Six circles, each as wide as the ring, centred around it
ringCircles :: Loc -> Double -> Double -> [(Loc, Double)]
ringCircles (Loc lat lon) inner outer =
    [ (Loc (lat + d * cos a / 111320) (lon + d * sin a / (111320 * cos (lat * pi / 180))), (outer - inner) / 2)
    | i <- [0 .. 5 :: Int]
    , let a = fromIntegral i * pi / 3
    ]
  where
    d = (inner + outer) / 2

-- | Places between the two radii, each once
inRing :: Loc -> Double -> Double -> [Restaurant] -> [Restaurant]
inRing centre inner outer = nubBy (\a b -> restaurantId a == restaurantId b) . filter within
  where
    within r = let m = metres centre (location r) in m >= inner && m <= outer

-- | First of each list, then second of each, and so on
interleave :: [[a]] -> [a]
interleave = concat . transpose

-- | Distance in metres, flat approximation that is fine within 50km
metres :: Loc -> Loc -> Double
metres (Loc lat lon) (Loc lat' lon') =
    let dy = (lat' - lat) * 111320
        dx = (lon' - lon) * 111320 * cos (lat * pi / 180)
     in sqrt (dx * dx + dy * dy)

-- | Photos kept per place, of the up to 10 Google returns
maxPhotos :: Int
maxPhotos = 5

{- | Fields shown on the card. Google bills a request at the most expensive
field asked for, and rating and priceLevel put this in the Enterprise SKU.
-}
fieldMask :: BS.ByteString
fieldMask = "places.id,places.displayName,places.location,places.rating,places.priceLevel,places.googleMapsUri,places.primaryTypeDisplayName,places.photos,places.generativeSummary,places.editorialSummary"

{- | The key goes in a header rather than the url so it can't end up in
logs. http-client only hides Authorization when it shows a request (as
in a logged error), so this header is added to the ones it hides.
-}
withKey :: PlacesKey -> [Header] -> Request -> Request
withKey (PlacesKey k) headers req =
    (setRequestHeaders (("X-Goog-Api-Key", k) : headers) req)
        { redactHeaders = Set.insert "X-Goog-Api-Key" (redactHeaders req)
        }

-- | Up to 20 restaurants within the radius in metres, most popular first
searchNearby :: PlacesKey -> Loc -> Double -> IO (Either FetchError [Restaurant])
searchNearby key l radius =
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
            , "locationRestriction" .= object ["circle" .= object ["center" .= l, "radius" .= radius]]
            ]

-- | Up to 20 places matching the vibe within the radius in metres
searchText :: PlacesKey -> Loc -> [Chip] -> Double -> IO (Either FetchError [Restaurant])
searchText key l chips radius =
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
              "locationRestriction" .= object ["rectangle" .= boxAround l radius]
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
            <*> parseSummary p
            <*> p .: "location"
    -- Google's AI summary where there is one, otherwise its editorial summary
    parseSummary p = do
        generated <- p .:? "generativeSummary"
        ai <- case generated of
            Nothing -> pure Nothing
            Just g -> do
                overview <- g .:? "overview" >>= traverse (.: "text")
                disclosure <- g .:? "disclosureText" >>= traverse (.: "text")
                pure (flip Summary disclosure <$> overview)
        editorial <- p .:? "editorialSummary" >>= traverse (.: "text")
        pure (maybe (flip Summary Nothing <$> editorial) Just ai)
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
