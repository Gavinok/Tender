-- | Restaurants from OpenStreetMap through the Overpass API. Free and needs
-- | no key, but has no photos or ratings.
module OpenStreetMap (openStreetMap, searchOverpass, parseOverpass, nearest) where

import Control.Applicative ((<|>))
import Data.Aeson (Value, withObject, (.!=), (.:), (.:?))
import qualified Data.Aeson.Types as AT
import Data.Char (toUpper)
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Fetch (FetchError, fetchJSON)
import Network.HTTP.Simple (parseRequest_, setRequestBodyURLEncoded, setRequestHeaders)
import Provider (Provider (..))
import Restaurant (Loc (..), Restaurant (..), Source (..))
import Vibe (Search (..))

openStreetMap :: Provider
openStreetMap = Provider "OpenStreetMap" $ \s -> searchOverpass (searchLoc s) (searchRadius s)

-- | The 20 nearest named restaurants within the radius in metres
searchOverpass :: Loc -> Double -> IO (Either FetchError [Restaurant])
searchOverpass l@(Loc lat lon) radius =
    fmap (nearest l 20) <$> fetchJSON parseOverpass request
  where
    request =
        -- Overpass asks clients to identify themselves
        setRequestHeaders [("User-Agent", "Tender (https://github.com/Gavinok/Tender)")] $
            setRequestBodyURLEncoded [("data", TE.encodeUtf8 query)] $
                parseRequest_ "POST https://overpass-api.de/api/interpreter"
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
nearest (Loc lat lon) n = take n . sortOn distance
  where
    -- Flat approximation, plenty for sorting within a few km
    distance r =
        let Loc lat' lon' = location r
            dx = (lon' - lon) * cos (lat * pi / 180)
            dy = lat' - lat
         in dx * dx + dy * dy

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
                    , restaurantId = "osm:" <> path
                    , name = n
                    , url = Just ("https://www.openstreetmap.org/" <> path)
                    , rating = Nothing
                    , price = Nothing
                    , category = cuisine >>= cuisineName
                    , photos = []
                    , summary = Nothing
                    , location = pos
                    }

-- | "vietnamese;noodle" becomes "Vietnamese"
cuisineName :: Text -> Maybe Text
cuisineName c = case T.strip (T.replace "_" " " (T.takeWhile (/= ';') c)) of
    "" -> Nothing
    t -> Just (T.cons (toUpper (T.head t)) (T.tail t))
