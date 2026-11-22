{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Restaurants as sent to the frontend. They come fresh from a source
-- | (Google Places, Yelp or OpenStreetMap) for each search and are not
-- | stored, since Google's and Yelp's terms only allow keeping ids.
module Restaurant (Restaurant (..), Source (..), Photo (..), PhotoSource (..), Author (..), Summary (..), Loc (..)) where

import Data.Aeson (FromJSON, ToJSON (toJSON), object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)

data Loc = Loc
    { latitude :: Double
    , longitude :: Double
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

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
    = -- | Google's resource name, places/<place>/photos/<photo>, fetched
      -- through /photo so the key stays on the server. Expires, so it is
      -- only good for the session that searched.
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
    , -- | Google asks for this to be shown with its AI summaries, e.g. "Summarized with Gemini"
      summaryDisclosure :: Maybe Text
    }
    deriving stock (Eq, Show)

data Restaurant = Restaurant
    { source :: Source
    , restaurantId :: Text
    , name :: Text
    , -- | The place's page on the source, e.g. Google Maps
      url :: Maybe Text
    , rating :: Maybe Double
    , -- | $ to $$$$
      price :: Maybe Text
    , category :: Maybe Text
    , -- | The first is shown on the card first
      photos :: [Photo]
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
