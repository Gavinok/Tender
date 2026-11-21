module Tender.Types
  ( ErrorMessage
  , Loc
  , Restaurant
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

type Restaurant =
  { id :: RestaurantId
  , name :: String
  , url :: Url
  , price :: Maybe String
  , rating :: Number
  , imageLink :: Url
  , category :: Maybe String
  }

-- | Location of the current user
type Loc = { latitude :: Number, longitude :: Number }
