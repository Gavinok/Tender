-- | Restaurants and photos from the Google Places API (New)
module Places (
    PlacesKey (..),
    withKey,
    googlePlaces,
    searchNearby,
    searchText,
    ringCircles,
    inRing,
    rankFor,
    photoUri,
    parsePlaces,
    isPhotoName,
    findPlaces,
    parseFound,
) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (Value, object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types qualified as AT
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum, toUpper)
import Data.Either (partitionEithers)
import Data.List (nubBy, sortOn, transpose)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Traversable (for)
import Fetch (FetchError, fetchJSON)
import Network.HTTP.Client (redactHeaders)
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
import Restaurant (Author (..), Loc (..), Photo (..), PhotoSource (..), Price (..), Restaurant (..), Review (..), Source (..), Summary (..), maxMoreCategories, metres)
import Vibe (Chip, Found (..), Place (..), Search (..), allChips, chipFields, chipId, priceLevels, queryTerms, wantsOpenNow)

newtype PlacesKey = PlacesKey BS.ByteString

-- | The most popular places nearby, or a text search when the group picked a vibe
googlePlaces :: PlacesKey -> Provider
googlePlaces key = Provider "Google Places" $ \(Search l chips radius inner) ->
    let search centre r = if null chips then searchNearby key centre r else searchText key centre chips r
     in fmap (rankFor chips)
            <$> if inner <= 0
                then search l radius
                else ringSearch l inner radius search

{- | Places Google confirms have every chosen chip first, and ones it says
lack one last. A place Google is silent about stays in between, since it
often leaves these out. Otherwise the order is kept.
-}
rankFor :: [Chip] -> [Restaurant] -> [Restaurant]
rankFor chips = sortOn rank
  where
    wanted = [chipId c | c <- chips, not (null (chipFields c))]
    rank r =
        let said = [v | (c, v) <- traits r, c `elem` wanted]
         in if any not said then 2 else if length (filter id said) == length wanted then 0 else 1 :: Int

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

-- | Photos kept per place, of the up to 10 Google returns
maxPhotos :: Int
maxPhotos = 5

{- | Fields shown on the card. Google bills a request at the most expensive
field asked for, and rating and priceLevel put this in the Enterprise SKU.
-}
fieldMask :: BS.ByteString
fieldMask = "places.id,places.displayName,places.location,places.rating,places.priceLevel,places.googleMapsUri,places.primaryTypeDisplayName,places.types,places.photos,places.generativeSummary,places.editorialSummary,places.reviews," <> attributeFields

-- | The attributes the chips can be confirmed by, in the same billing tier as the summaries
attributeFields :: BS.ByteString
attributeFields = BS.intercalate "," [TE.encodeUtf8 ("places." <> f) | f <- concatMap chipFields allChips]

{- | The key goes in a header rather than the url so it can't end up in
logs. http-client only hides Authorization when it shows a request (as
in a logged error), so this header is added to the ones it hides.
-}
withKey :: PlacesKey -> [Header] -> Request -> Request
withKey (PlacesKey k) headers req =
    (setRequestHeaders (("X-Goog-Api-Key", k) : headers) req)
        { redactHeaders = Set.insert "X-Goog-Api-Key" (redactHeaders req)
        }

-- | Seconds to wait for a search before falling back to the next source
searchTimeout :: Int
searchTimeout = 10

-- | Up to 20 restaurants within the radius in metres, most popular first
searchNearby :: PlacesKey -> Loc -> Double -> IO (Either FetchError [Restaurant])
searchNearby key l radius =
    fetchJSON searchTimeout parsePlaces
        $ withKey key [("X-Goog-FieldMask", fieldMask)]
        $ setRequestBodyJSON body
        $ parseRequest_ "POST https://places.googleapis.com/v1/places:searchNearby"
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
    fetchJSON searchTimeout parsePlaces
        $ withKey key [("X-Goog-FieldMask", fieldMask)]
        $ setRequestBodyJSON body
        $ parseRequest_ "POST https://places.googleapis.com/v1/places:searchText"
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

{- | Up to 5 places matching what the group typed for where they're eating,
e.g. a neighbourhood, address or city. Asks only for names, addresses and
locations.
-}
findPlaces :: PlacesKey -> Text -> IO (Either FetchError [Found])
findPlaces key q =
    fetchJSON searchTimeout parseFound
        $ withKey key [("X-Goog-FieldMask", "places.displayName,places.formattedAddress,places.location")]
        $ setRequestBodyJSON (object ["textQuery" .= q, "pageSize" .= (5 :: Int)])
        $ parseRequest_ "POST https://places.googleapis.com/v1/places:searchText"

parseFound :: Value -> AT.Parser [Found]
parseFound = withObject "SearchTextResponse" $ \o ->
    o .:? "places" .!= [] >>= traverse found
  where
    found = withObject "Place" $ \p -> do
        n <- p .: "displayName" >>= (.: "text")
        address <- p .:? "formattedAddress" .!= ""
        l <- p .: "location"
        pure (Found (Place n l) address)

-- | A short lived url for the photo, at most the given width
photoUri :: PlacesKey -> Text -> Int -> IO (Either FetchError Text)
photoUri key resource width =
    -- Holds up a card's photo, so shorter than searches
    fetchJSON 5 (withObject "Photo" (.: "photoUri"))
        $ withKey key []
        $ setRequestQueryString [("maxWidthPx", Just (TE.encodeUtf8 (T.pack (show width)))), ("skipHttpRedirect", Just "true")]
        $ setRequestPath (TE.encodeUtf8 ("/v1/" <> resource <> "/media"))
        $ parseRequest_ "GET https://places.googleapis.com"

{- | Whether the text has the shape of a photo's resource name. The photo
endpoint only passes these on, so it can't be used to call anything else.
-}
isPhotoName :: Text -> Bool
isPhotoName t = case T.splitOn "/" t of
    ["places", place, "photos", ref] -> all segment [place, ref]
    _ -> False
  where
    segment s = not (T.null s) && T.all (\c -> isAlphaNum c || c == '-' || c == '_') s

{- | Google's types other than the generic ones and the one already shown,
| e.g. "bar" becomes "Bar"
-}
moreTypes :: Maybe Text -> [Text] -> [Text]
moreTypes primary types =
    take
        maxMoreCategories
        [ label
        | t <- types
        , t `notElem` genericTypes
        , let label = typeLabel t
        , T.toLower label /= maybe "" T.toLower primary
        ]
  where
    genericTypes = ["restaurant", "food", "point_of_interest", "establishment", "store", "meal_takeaway", "meal_delivery"]
    typeLabel t = case T.uncons (T.replace "_" " " t) of
        Just (c, rest) -> T.cons (toUpper c) rest
        Nothing -> t

parsePlaces :: Value -> AT.Parser [Restaurant]
parsePlaces = withObject "SearchNearbyResponse" $ \o ->
    -- Google leaves the field out when nothing is found
    o .:? "places" .!= [] >>= traverse place
  where
    place = withObject "Place" $ \p -> do
        found <- p .:? "photos" .!= []
        primary <- p .:? "primaryTypeDisplayName" >>= traverse (.: "text")
        Restaurant Google
            <$> p .: "id"
            <*> (p .: "displayName" >>= (.: "text"))
            <*> p .:? "googleMapsUri"
            <*> p .:? "rating"
            <*> ((>>= priceFromLevel) <$> p .:? "priceLevel")
            <*> pure primary
            <*> (moreTypes primary <$> p .:? "types" .!= [])
            -- Each photo shown is a billed request, so only the first few
            <*> traverse parsePhoto (take maxPhotos found)
            <*> parseSummary p
            <*> (p .:? "reviews" .!= [] >>= fmap catMaybes . traverse parseReview)
            <*> parseTraits p
            <*> p .: "location"
    -- A chip holds when any of its attributes is true, and fails when Google
    -- gave some and none were
    parseTraits p = fmap catMaybes . for allChips $ \c -> do
        said <- traverse (\f -> p .:? Key.fromText f) (chipFields c) :: AT.Parser [Maybe Bool]
        pure $ case catMaybes said of
            [] -> Nothing
            vs -> Just (chipId c, or vs)
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
        pure (ai <|> (flip Summary Nothing <$> editorial))
    -- Google also sends reviews with a rating and no words, which have nothing to show
    parseReview = withObject "Review" $ \v -> do
        body <- v .:? "text" >>= traverse (.: "text")
        author <- v .: "authorAttribution" >>= parseAuthor
        rate <- v .:? "rating"
        age <- v .:? "relativePublishTimeDescription"
        pure $ case T.strip <$> body of
            Just t | not (T.null t) -> Just (Review author rate t age)
            _ -> Nothing
    parsePhoto = withObject "Photo" $ \ph -> do
        authors <- ph .:? "authorAttributions" .!= []
        Photo . GooglePhoto <$> ph .: "name" <*> traverse parseAuthor (listToMaybe authors)
    parseAuthor = withObject "AuthorAttribution" $ \a ->
        Author <$> a .: "displayName" <*> (fmap absolute <$> a .:? "uri")
    -- Author links come back without a scheme, e.g. //maps.google.com/...
    absolute u = if "//" `T.isPrefixOf` u then "https:" <> u else u

priceFromLevel :: Text -> Maybe Price
priceFromLevel = \case
    "PRICE_LEVEL_FREE" -> Just Free
    "PRICE_LEVEL_INEXPENSIVE" -> Just Inexpensive
    "PRICE_LEVEL_MODERATE" -> Just Moderate
    "PRICE_LEVEL_EXPENSIVE" -> Just Expensive
    "PRICE_LEVEL_VERY_EXPENSIVE" -> Just VeryExpensive
    _ -> Nothing
