module Tender.App
  ( Message(..)
  , Model(..)
  , Screen(..)
  , Swiper
  , init
  , update
  ) where

import Prelude

import Data.Array (cons, elem)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff, attempt, message)
import Effect.Class (liftEffect)
import Flame (ListUpdate, (:>))
import Tender.Api (ApiError(..), createSession, getRestaurant, joinSession, sendLiked)
import Tender.Browser (existingSession, geolocation, shareLink)
import Tender.Types (ErrorMessage, Loc, Restaurant, RestaurantId, SessionId, Url, UserId)

-- | What we know once the user has joined a session
type Swiper =
  { user :: UserId
  , loc :: Loc
  , matches :: Array Restaurant
  }

-- | What a user who has joined a session is looking at
data Screen
  = Fetching
  | Swiping Restaurant
  | Matched Restaurant
  | NoneLeft

-- | The model represents the state of the app
data Model
  = Loading
  -- | Waiting to start, with the link others can use to join
  | QR SessionId Url
  | Active Swiper Screen
  | ServerError ErrorMessage

derive instance Eq Screen
derive instance Generic Screen _
instance Show Screen where
  show = genericShow

derive instance Eq Model
derive instance Generic Model _
instance Show Model where
  show = genericShow

-- | This datatype is used to signal events to `update`
data Message
  = DetermineSession
  | JoinSession SessionId Url
  | StartSwiping
  | Started UserId Loc
  | Like
  | Dislike
  -- | Skip past the current match and keep swiping
  | Continue
  | Show Restaurant
  | GotMatch Restaurant
  | OutOfRestaurants
  | FailedToLoad ErrorMessage

-- | Ask for the restaurant after the given one
nextRestaurant :: Loc -> Maybe RestaurantId -> Aff (Maybe Message)
nextRestaurant loc after = Just <<< toMessage <$> getRestaurant loc after
  where
  toMessage = case _ of
    Left (NotFound _) -> OutOfRestaurants
    Left (Failed e) -> FailedToLoad e
    Right r -> Show r

-- | Show a failed request to the user
failed :: ApiError -> Message
failed = case _ of
  NotFound e -> FailedToLoad e
  Failed e -> FailedToLoad e

orFail :: forall a. (a -> Message) -> Either ApiError a -> Message
orFail = either failed

-- | Initial state of the app
init :: Model
init = Loading

-- | `update` is called to handle events
update :: ListUpdate Model Message
update model = case model, _ of
  _, FailedToLoad e -> ServerError e :> []

  _, DetermineSession -> model :>
    [ Just <$> do
        found <- liftEffect existingSession
        session <- maybe createSession (pure <<< Right) found
        case session of
          Left e -> pure $ failed e
          Right s -> JoinSession s <$> liftEffect (shareLink s)
    ]
  _, JoinSession session link -> QR session link :> []

  QR session _, StartSwiping -> model :>
    [ attempt geolocation >>= case _ of
        Left e -> pure $ Just $ FailedToLoad $ "Could not get your location: " <> message e
        Right loc -> Just <<< orFail (\u -> Started u loc) <$> joinSession session
    ]
  _, Started user loc ->
    Active { user, loc, matches: [] } Fetching :> [ nextRestaurant loc Nothing ]

  Active swiper (Swiping r), Like -> model :>
    [ do
        match <- sendLiked swiper.user (Just r.id)
        case match of
          Left e -> pure $ Just $ failed e
          Right (Just m) | not (elem m.id (_.id <$> swiper.matches)) -> pure $ Just $ GotMatch m
          Right _ -> nextRestaurant swiper.loc (Just r.id)
    ]
  Active swiper (Swiping r), Dislike -> model :>
    [ sendLiked swiper.user Nothing *> nextRestaurant swiper.loc (Just r.id) ]
  Active swiper (Matched r), Continue -> model :>
    [ nextRestaurant swiper.loc (Just r.id) ]

  Active swiper _, Show r -> Active swiper (Swiping r) :> []
  Active swiper _, GotMatch r ->
    Active swiper { matches = cons r swiper.matches } (Matched r) :> []
  Active swiper _, OutOfRestaurants -> Active swiper NoneLeft :> []

  -- Anything else is a stale message, e.g. from a double click
  _, _ -> model :> []
