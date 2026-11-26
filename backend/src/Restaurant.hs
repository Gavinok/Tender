{-# LANGUAGE DeriveAnyClass #-}

-- | Restaurants as sent to the frontend. They come fresh from a source
-- | (Google Places, Yelp or OpenStreetMap) for each search and are not
-- | stored, since Google's and Yelp's terms only allow keeping ids.
module Restaurant (Restaurant (..), Source (..), Photo (..), PhotoSource (..), Author (..), Summary (..), Loc (..), metres, RestaurantId (..), Price (..), priceFromSymbol) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.Text (Text)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import GHC.Generics (Generic)

data Loc = Loc
    { latitude :: Double
    , longitude :: Double
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (ToJSON)

{- | Only real places: anything else would become the session's location and
fail at every source, after counting against the search limits. aeson
reads null as NaN, so finite is checked too.
-}
instance FromJSON Loc where
    parseJSON = withObject "Loc" $ \o -> do
        lat <- o .: "latitude"
        lon <- o .: "longitude"
        let within limit x = not (isNaN x || isInfinite x) && abs x <= limit
        if within 90 lat && within 180 lon
            then pure (Loc lat lon)
            else fail "latitude must be within ±90 and longitude within ±180"

{- | A place's id at its source: Google's place id, Yelp's business id or
osm:<type>/<id>. Decisions are stored by it.
-}
newtype RestaurantId = RestaurantId Text
    deriving stock (Eq, Ord, Show)
    deriving newtype (FromJSON, ToJSON, FromField, ToField)

-- | How expensive a place is, shown as Free or $ to $$$$
data Price = Free | Inexpensive | Moderate | Expensive | VeryExpensive
    deriving stock (Bounded, Enum, Eq, Ord, Show)

instance ToJSON Price where
    toJSON = toJSON . priceSymbol

priceSymbol :: Price -> Text
priceSymbol = \case
    Free -> "Free"
    Inexpensive -> "$"
    Moderate -> "$$"
    Expensive -> "$$$"
    VeryExpensive -> "$$$$"

-- | From the symbols Yelp uses, "$" to "$$$$"
priceFromSymbol :: Text -> Maybe Price
priceFromSymbol t = lookup t [(priceSymbol p, p) | p <- [minBound .. maxBound]]

-- | Distance in metres, flat approximation that is fine within 50km
metres :: Loc -> Loc -> Double
metres (Loc lat lon) (Loc lat' lon') =
    let dy = (lat' - lat) * 111320
        dx = (lon' - lon) * 111320 * cos (lat * pi / 180)
     in sqrt (dx * dx + dy * dy)

-- | Who took a photo, which has to be shown with it
data Author = Author
    { authorName :: Text
    , authorUri :: Maybe Text
    }
    deriving stock (Eq, Show)

-- | Where a restaurant came from, which decides the attribution shown
data Source = Google | Yelp | OpenStreetMap
    deriving stock (Eq, Show)

data PhotoSource
    = {- | Google's resource name, places/<place>/photos/<photo>, fetched
      through /photo so the key stays on the server. Expires, so it is
      only good for the session that searched.
      -}
      GooglePhoto Text
    | -- | A public image url
      PhotoUrl Text
    deriving stock (Eq, Show)

data Photo = Photo
    { photoSource :: PhotoSource
    , photoAuthor :: Maybe Author
    }
    deriving stock (Eq, Show)

-- | A few sentences about the place
data Summary = Summary
    { summaryText :: Text
    , summaryDisclosure :: Maybe Text
    -- ^ Google asks for this to be shown with its AI summaries, e.g. "Summarized with Gemini"
    }
    deriving stock (Eq, Show)

data Restaurant = Restaurant
    { source :: Source
    , restaurantId :: RestaurantId
    , name :: Text
    , url :: Maybe Text
    -- ^ The place's page on the source, e.g. Google Maps
    , rating :: Maybe Double
    , price :: Maybe Price
    -- ^ \$ to $$$$
    , category :: Maybe Text
    , photos :: [Photo]
    -- ^ The first is shown on the card first
    , summary :: Maybe Summary
    , location :: Loc
    }
    deriving stock (Eq, Show)

instance ToJSON Restaurant where
    toJSON r =
        object
            [ "source" .= sourceName (source r)
            , "id" .= restaurantId r
            , "name" .= name r
            , "url" .= url r
            , "rating" .= rating r
            , "price" .= price r
            , "category" .= category r
            , "photos" .= map photoJSON (photos r)
            , "summary" .= fmap (\s -> object ["text" .= summaryText s, "disclosure" .= summaryDisclosure s]) (summary r)
            ]
      where
        sourceName :: Source -> Text
        sourceName = \case
            Google -> "google"
            Yelp -> "yelp"
            OpenStreetMap -> "openstreetmap"
        photoJSON p =
            object $
                ("author" .= fmap authorJSON (photoAuthor p)) : case photoSource p of
                    GooglePhoto n -> ["name" .= n]
                    PhotoUrl u -> ["url" .= u]
        authorJSON a = object ["name" .= authorName a, "uri" .= authorUri a]
