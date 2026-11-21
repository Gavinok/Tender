module Tender.App
  ( Message(..)
  , Model(..)
  , Screen(..)
  , Lobby
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
import Effect.Aff (Aff, Milliseconds(..), attempt, delay, message)
import Effect.Class (liftEffect)
import Data.Tuple.Nested ((/\))
import Flame (Update)
import Tender.Api (ApiError(..), createSession, decide, getRestaurant, joinSession)
import Tender.Browser (Shared(..), existingSession, geolocation, reload, rememberSession, shareLink, shareOrCopy, vibrate)
import Tender.Types (ErrorMessage, Loc, Restaurant, RestaurantId, SessionId, Url, UserId)

-- | What we know once the user has joined a session
type Swiper =
  { user :: UserId
  , loc :: Loc
  , matches :: Array Restaurant
  -- | Whether the list of matches is open
  , showMatches :: Boolean
  }

-- | What a user who has joined a session is looking at
data Screen
  = Fetching
  | Swiping Restaurant
  | Matched Restaurant
  | NoneLeft

type Lobby =
  { session :: SessionId
  , link :: Url
  -- | Show that the link was just copied
  , copied :: Boolean
  }

-- | The model represents the state of the app
data Model
  = Loading
  -- | Waiting to start, with the link others can use to join
  | QR Lobby
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
  -- | A key pressed anywhere on the page
  | KeyPressed String
  -- | Skip past the current match and keep swiping
  | Continue
  | Show Restaurant
  | GotMatch Restaurant
  | OutOfRestaurants
  | FailedToLoad ErrorMessage
  -- | Start again from the session in the url
  | Retry
  | ToggleMatches
  | ShareInvite
  | LinkCopied
  | HideCopied

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
update :: Update Model Message
update model = case model, _ of
  _, FailedToLoad e -> ServerError e /\ []
  _, Retry -> model /\ [ liftEffect reload $> Nothing ]

  _, DetermineSession -> model /\
    [ Just <$> do
        found <- liftEffect existingSession
        session <- maybe createSession (pure <<< Right) found
        case session of
          Left e -> pure $ failed e
          Right s -> liftEffect do
            rememberSession s
            JoinSession s <$> shareLink s
    ]
  _, JoinSession session link -> QR { session, link, copied: false } /\ []
  QR { link }, ShareInvite -> model /\
    [ attempt (shareOrCopy link) <#> case _ of
        Right Copied -> Just LinkCopied
        _ -> Nothing
    ]
  QR lobby, LinkCopied ->
    QR lobby { copied = true } /\ [ delay (Milliseconds 2000.0) $> Just HideCopied ]
  QR lobby, HideCopied -> QR lobby { copied = false } /\ []

  QR { session }, StartSwiping -> model /\
    [ attempt geolocation >>= case _ of
        Left e -> pure $ Just $ FailedToLoad $ "Could not get your location: " <> message e
        Right loc -> Just <<< orFail (\u -> Started u loc) <$> joinSession session
    ]
  _, Started user loc ->
    Active { user, loc, matches: [], showMatches: false } Fetching /\ [ nextRestaurant loc Nothing ]

  Active swiper (Swiping r), Like -> model /\
    [ do
        match <- decide swiper.user r.id true
        case match of
          Left e -> pure $ Just $ failed e
          Right (Just m) | not (elem m.id (_.id <$> swiper.matches)) -> pure $ Just $ GotMatch m
          Right _ -> nextRestaurant swiper.loc (Just r.id)
    ]
  Active swiper (Swiping r), Dislike -> model /\
    [ decide swiper.user r.id false >>= case _ of
        Left e -> pure $ Just $ failed e
        Right _ -> nextRestaurant swiper.loc (Just r.id)
    ]
  Active _ (Swiping _), KeyPressed "ArrowRight" -> update model Like
  Active _ (Swiping _), KeyPressed "ArrowLeft" -> update model Dislike

  Active swiper (Matched r), Continue -> model /\
    [ nextRestaurant swiper.loc (Just r.id) ]

  Active swiper _, Show r -> Active swiper (Swiping r) /\ []
  Active swiper _, GotMatch r ->
    Active swiper { matches = cons r swiper.matches } (Matched r) /\ [ liftEffect vibrate $> Nothing ]
  Active swiper screen, ToggleMatches ->
    Active swiper { showMatches = not swiper.showMatches } screen /\ []
  Active swiper _, OutOfRestaurants -> Active swiper NoneLeft /\ []

  -- Anything else is a stale message, e.g. from a double click
  _, _ -> model /\ []
