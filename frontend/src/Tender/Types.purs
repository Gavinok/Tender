module Tender.Types
  ( ErrorMessage
  , Author
  , Review
  , Loc
  , Place
  , Found
  , Person
  , Status(..)
  , Photo
  , Restaurant
  , photoUrl
  , RestaurantId(..)
  , SessionId(..)
  , Url
  , UserId(..)
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign (ForeignError(..), fail)
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl)

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

-- | Who took a photo or wrote a review. Google requires showing this with it.
type Author = { name :: String, uri :: Maybe Url }

-- | A Google photo's resource name, or the url of a public image
type Photo = { name :: Maybe String, url :: Maybe Url, author :: Maybe Author }

-- | What a visitor wrote, as Google words its age ("a month ago")
type Review = { author :: Author, rating :: Maybe Number, text :: String, when :: Maybe String }

-- | From Google Places, Yelp or OpenStreetMap, as given by `source`. Kept
-- | only for the session, as Google's and Yelp's terms require.
type Restaurant =
  { source :: String
  , id :: RestaurantId
  , name :: String
  -- | The place on Google Maps
  , url :: Maybe Url
  , price :: Maybe String
  , rating :: Maybe Number
  , category :: Maybe String
  -- | Other kinds of place it is, after `category`
  , moreCategories :: Array String
  -- | The first is shown first
  , photos :: Array Photo
  -- | A few sentences about the place, with Google's disclosure for AI summaries
  , summary :: Maybe { text :: String, disclosure :: Maybe String }
  -- | A few of the reviews the summary was drawn from
  , reviews :: Array Review
  -- | Ids of the chips Google confirms the place has
  , confirmed :: Array String
  }

-- | Where to load the photo from. Google photos go through the backend, which
-- | redirects to the image and keeps the API key off the page. Their resource
-- | names only contain letters, digits, - and _ besides the slashes, so they
-- | don't need escaping.
photoUrl :: Photo -> Maybe Url
photoUrl p = case p.url, p.name of
  Just url, _ -> Just url
  Nothing, Just name -> Just ("/api/photo?name=" <> name)
  Nothing, Nothing -> Nothing

-- | Location of the current user
type Loc = { latitude :: Number, longitude :: Number }

-- | Where the group said they're eating
type Place = { name :: String, latitude :: Number, longitude :: Number }

-- | A place a lookup found, with its address to tell it apart from others
type Found = { name :: String, address :: String, latitude :: Number, longitude :: Number }

-- | What someone in the session is up to, as of their last heartbeat
data Status = Swiping | Done | Away

derive instance Eq Status

instance Show Status where
  show = case _ of
    Swiping -> "Swiping"
    Done -> "Done"
    Away -> "Away"

instance ReadForeign Status where
  readImpl f = readImpl f >>= case _ of
    "swiping" -> pure Swiping
    "done" -> pure Done
    "away" -> pure Away
    other -> fail (ForeignError ("Unknown status: " <> other))

-- | Someone in the session. `left` is Nothing until their first search is back.
type Person = { you :: Boolean, status :: Status, swiped :: Int, left :: Maybe Int }
