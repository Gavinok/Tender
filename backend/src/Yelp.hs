-- | Restaurants from Yelp's business search
module Yelp (YelpKey (..), yelp, searchYelp, parseYelp) where

import Data.Aeson (Value, withObject, (.!=), (.:), (.:?))
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as S8
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Fetch (FetchError, fetchJSON)
import Network.HTTP.Simple (parseRequest_, setRequestHeaders, setRequestQueryString)
import Provider (Provider (..))
import Restaurant (Loc (..), Photo (..), PhotoSource (..), Restaurant (..), Source (..))
import Vibe (Search (..))

newtype YelpKey = YelpKey BS.ByteString

yelp :: YelpKey -> Provider
yelp key = Provider "Yelp" (searchYelp key . searchLoc)

-- | Up to 20 restaurants within 5km, Yelp's best matches first
searchYelp :: YelpKey -> Loc -> IO (Either FetchError [Restaurant])
searchYelp (YelpKey key) (Loc lat lon) =
    fetchJSON parseYelp $
        setRequestHeaders [("Authorization", "Bearer " <> key)] $
            setRequestQueryString
                [ ("latitude", Just (S8.pack (show lat)))
                , ("longitude", Just (S8.pack (show lon)))
                , ("categories", Just "restaurants")
                , ("radius", Just "5000")
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
            <*> b .:? "price"
            <*> traverse (withObject "Category" (.: "title")) (listToMaybe categories)
            <*> (fmap urlPhoto . nonEmpty <$> b .:? "image_url")
            <*> (Loc <$> coords .: "latitude" <*> coords .: "longitude")
    urlPhoto u = Photo (PhotoUrl u) Nothing
    -- Yelp sends "" for businesses without a photo
    nonEmpty :: Maybe Text -> Maybe Text
    nonEmpty m = m >>= \t -> if T.null t then Nothing else Just t
