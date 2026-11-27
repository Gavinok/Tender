-- | Restaurants from Yelp's business search
module Yelp (YelpKey (..), yelp, searchYelp, parseYelp) where

import Data.Aeson (Value, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types qualified as AT
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as S8
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Fetch (FetchError, fetchJSON)
import Network.HTTP.Simple (parseRequest_, setRequestHeaders, setRequestQueryString)
import Provider (Provider (..))
import Restaurant (Loc (..), Photo (..), PhotoSource (..), Restaurant (..), Source (..), maxMoreCategories, priceFromSymbol)
import Vibe (Search (..))

newtype YelpKey = YelpKey BS.ByteString

yelp :: YelpKey -> Provider
yelp key = Provider "Yelp" $ \s -> searchYelp key (searchLoc s) (searchRadius s)

-- | Up to 20 restaurants within the radius in metres, Yelp's best matches first
searchYelp :: YelpKey -> Loc -> Double -> IO (Either FetchError [Restaurant])
searchYelp (YelpKey key) (Loc lat lon) radius =
    fetchJSON 10 parseYelp
        $ setRequestHeaders [("Authorization", "Bearer " <> key)]
        $ setRequestQueryString
            [ ("latitude", Just (S8.pack (show lat)))
            , ("longitude", Just (S8.pack (show lon)))
            , ("categories", Just "restaurants")
            , -- Yelp's limit is 40km
              ("radius", Just (S8.pack (show (round (min 40000 radius) :: Int))))
            , ("limit", Just "20")
            , ("sort_by", Just "best_match")
            ]
            (parseRequest_ "GET https://api.yelp.com/v3/businesses/search")

parseYelp :: Value -> AT.Parser [Restaurant]
parseYelp = withObject "BusinessSearchResponse" $ \o ->
    o .:? "businesses" .!= [] >>= traverse business
  where
    business = withObject "Business" $ \b -> do
        categories <- b .:? "categories" .!= []
        coords <- b .: "coordinates"
        Restaurant Yelp
            <$> b .: "id"
            <*> b .: "name"
            <*> b .:? "url"
            <*> b .:? "rating"
            <*> ((>>= priceFromSymbol) <$> b .:? "price")
            <*> pure Nothing
            <*> traverse (withObject "Category" (.: "title")) (listToMaybe categories)
            <*> traverse (withObject "Category" (.: "title")) (take maxMoreCategories (drop 1 categories))
            <*> (maybe [] (pure . urlPhoto) . nonEmpty <$> b .:? "image_url")
            <*> pure Nothing
            <*> pure []
            <*> pure []
            <*> (Loc <$> coords .: "latitude" <*> coords .: "longitude")
            <*> ((b .:? "location") >>= maybe (pure Nothing) (.:? "address1"))
            <*> pure Nothing
            <*> pure Nothing
    urlPhoto u = Photo (PhotoUrl u) Nothing
    -- Yelp sends "" for businesses without a photo
    nonEmpty :: Maybe Text -> Maybe Text
    nonEmpty m = m >>= \t -> if T.null t then Nothing else Just t
