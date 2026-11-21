module Tender.Types
  ( ErrorMessage
  , Author
  , Loc
  , Photo
  , Restaurant
  , photoUrl
  , RestaurantId(..)
  , SessionId(..)
  , Url
  , UserId(..)
  ) where

import Prelude

import Data.Maybe (Maybe)
import Yoga.JSON (class ReadForeign, class WriteForeign)

newtype UserId = UserId String

derive newtype instance Eq UserId
derive newtype instance Show UserId
derive newtype instance ReadForeign UserId
derive newtype instance WriteForeign UserId

newtype SessionId = SessionId String

derive newtype instance Eq SessionId
derive newtype instance Show SessionId
derive newtype instance ReadForeign SessionId

newtype RestaurantId = RestaurantId String

derive newtype instance Eq RestaurantId
derive newtype instance Show RestaurantId
derive newtype instance ReadForeign RestaurantId
derive newtype instance WriteForeign RestaurantId

type ErrorMessage = String

type Url = String

-- | Who took a photo. Google requires showing this with the photo.
type Author = { name :: String, uri :: Maybe Url }

-- | A photo from Google Places, by its resource name
type Photo = { name :: String, author :: Maybe Author }

-- | From Google Places. Kept only for the session, as Google's terms require.
type Restaurant =
  { id :: RestaurantId
  , name :: String
  -- | The place on Google Maps
  , url :: Maybe Url
  , price :: Maybe String
  , rating :: Maybe Number
  , category :: Maybe String
  , photo :: Maybe Photo
  }

-- | The backend redirects this to the image, keeping the API key off the page.
-- | Resource names only contain letters, digits, - and _ besides the slashes,
-- | so they don't need escaping.
photoUrl :: Photo -> Url
photoUrl p = "/api/photo?name=" <> p.name

-- | Location of the current user
type Loc = { latitude :: Number, longitude :: Number }
