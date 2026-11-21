{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Restaurants as sent to the frontend. They come fresh from Google Places
-- | for each search and are not stored, since Google's terms only allow
-- | keeping place ids.
module Restaurant (Restaurant (..), Photo (..), Author (..), Loc (..)) where

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

data Photo = Photo
    { -- | Google's resource name, places/<place>/photos/<photo>. Expires, so
      -- | it is only good for the session that searched.
      photoName :: Text
    , photoAuthor :: Maybe Author
    }
    deriving stock (Eq, Show)

data Restaurant = Restaurant
    { restaurantId :: Text
    , name :: Text
    , -- | Google Maps page for the place
      url :: Maybe Text
    , rating :: Maybe Double
    , -- | $ to $$$$
      price :: Maybe Text
    , category :: Maybe Text
    , photo :: Maybe Photo
    , location :: Loc
    }
    deriving stock (Eq, Show)

instance ToJSON Restaurant where
    toJSON r =
        object
            [ "id" .= restaurantId r
            , "name" .= name r
            , "url" .= url r
            , "rating" .= rating r
            , "price" .= price r
            , "category" .= category r
            , "photo" .= fmap photoJSON (photo r)
            ]
      where
        photoJSON p = object ["name" .= photoName p, "author" .= fmap authorJSON (photoAuthor p)]
        authorJSON a = object ["name" .= authorName a, "uri" .= authorUri a]
