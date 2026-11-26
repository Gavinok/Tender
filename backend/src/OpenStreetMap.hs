-- | Restaurants from OpenStreetMap through the Overpass API. Free and needs
-- | no key, but has no photos or ratings.
module OpenStreetMap (openStreetMap, searchOverpass, parseOverpass, nearest) where

import Control.Applicative ((<|>))
import Data.Aeson (Value, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types qualified as AT
import Data.Char (toUpper)
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Fetch (FetchError, fetchJSON)
import Network.HTTP.Simple (parseRequest_, setRequestBodyURLEncoded, setRequestHeaders)
import Provider (Provider (..))
import Restaurant (Loc (..), Restaurant (..), RestaurantId (..), Source (..), metres)
import Vibe (Search (..))

openStreetMap :: Provider
openStreetMap = Provider "OpenStreetMap" $ \s -> searchOverpass (searchLoc s) (searchInner s) (searchRadius s)

-- | The 20 nearest named restaurants between the inner radius (0 for none) and the radius, in metres
searchOverpass :: Loc -> Double -> Double -> IO (Either FetchError [Restaurant])
searchOverpass l@(Loc lat lon) inner radius =
    -- Places within the inner radius were already shown when searching further
    fmap (nearest l 20 . filter ((>= inner) . metres l . location)) <$> fetchJSON 25 parseOverpass request
  where
    -- The query tells Overpass to give up after 20 seconds; this allows a bit more
    request =
        -- Overpass asks clients to identify themselves
        setRequestHeaders [("User-Agent", "Tender (https://github.com/Gavinok/Tender)")]
            $ setRequestBodyURLEncoded [("data", TE.encodeUtf8 query)]
            $ parseRequest_ "POST https://overpass-api.de/api/interpreter"
    query =
        T.concat
            [ "[out:json][timeout:20];"
            , "nwr[\"amenity\"=\"restaurant\"][\"name\"](around:"
            , T.pack (show (round radius :: Int))
            , ","
            , T.pack (show lat)
            , ","
            , T.pack (show lon)
            , ");out center tags;"
            ]

-- | The closest restaurants first
nearest :: Loc -> Int -> [Restaurant] -> [Restaurant]
nearest l n = take n . sortOn (metres l . location)

parseOverpass :: Value -> AT.Parser [Restaurant]
parseOverpass = withObject "OverpassResponse" $ \o ->
    catMaybes <$> (o .:? "elements" .!= [] >>= traverse element)
  where
    element = withObject "Element" $ \e -> do
        kind <- e .: "type"
        osmId <- e .: "id" :: AT.Parser Integer
        tags <- e .:? "tags" .!= mempty
        name' <- tags .:? "name"
        cuisine <- tags .:? "cuisine"
        -- Nodes have a position, ways and relations a centre
        position <-
            (Just <$> (Loc <$> e .: "lat" <*> e .: "lon"))
                <|> (e .:? "center" >>= traverse (\c -> Loc <$> c .: "lat" <*> c .: "lon"))
        let path = kind <> "/" <> T.pack (show osmId)
        pure $ do
            n <- name'
            pos <- position
            pure $
                Restaurant
                    { source = OpenStreetMap
                    , restaurantId = RestaurantId ("osm:" <> path)
                    , name = n
                    , url = Just ("https://www.openstreetmap.org/" <> path)
                    , rating = Nothing
                    , price = Nothing
                    , category = cuisine >>= cuisineName
                    , photos = []
                    , summary = Nothing
                    , reviews = []
                    , location = pos
                    }

-- | "vietnamese;noodle" becomes "Vietnamese"
cuisineName :: Text -> Maybe Text
cuisineName c =
    (\(first, rest) -> T.cons (toUpper first) rest)
        <$> T.uncons (T.strip (T.replace "_" " " (T.takeWhile (/= ';') c)))
