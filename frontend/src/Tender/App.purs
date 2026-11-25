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

import Data.Array (cons, elem, filter, head, length, snoc, uncons)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff, Milliseconds(..), attempt, delay, message)
import Effect.Class (liftEffect)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Flame (Update)
import Tender.Api (ApiError(..), createSession, decide, getRestaurants, getVibe, joinSession, setVibe)
import Tender.Vibe (Vibe)
import Tender.Swipe (flyOut)
import Tender.Swipe as Swipe
import Tender.Browser (Shared(..), existingSession, geolocation, reload, rememberSession, rememberUser, rememberedUser, shareLink, shareOrCopy, startNewSession, vibrate)
import Tender.Types (ErrorMessage, Loc, Restaurant, RestaurantId, SessionId, Url, UserId)

-- | What we know once the user has joined a session
type Swiper =
  { user :: UserId
  , loc :: Loc
  -- | How far the session searches, in metres, and how far it can go
  , radius :: Number
  , maxRadius :: Number
  -- | Restaurants still to show, fetched once when swiping starts
  , upcoming :: Array Restaurant
  -- | Which of the current card's photos is showing
  , photo :: Int
  -- | Whether the current card's summary is expanded
  , summaryOpen :: Boolean
  -- | Restaurants someone else in the session liked, shown first and badged
  , othersLiked :: Array RestaurantId
  -- | Restaurants this person liked, newest first, to show when one becomes a match
  , liked :: Array Restaurant
  , matches :: Array Restaurant
  -- | Whether the list of matches is open
  , showMatches :: Boolean
  -- | The session's vibe chips
  , vibe :: Array String
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
  , vibe :: Vibe
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
  | Loaded { restaurants :: Array Restaurant, othersLiked :: Array RestaurantId, vibe :: Array String, radius :: Number, maxRadius :: Number }
  | SearchFurther
  | NextPhoto
  | PreviousPhoto
  | ToggleSummary
  | ToggleChip String
  -- | The vibe as saved after this person changed it
  | VibeSaved Vibe
  -- | The vibe as fetched by the lobby's regular check, which then schedules the next one
  | VibePolled Vibe
  | PollVibe
  -- | The decision on this restaurant is saved. Brings the session's matches
  -- | this person is part of, including ones others completed since.
  | Decided { id :: RestaurantId, liked :: Boolean, matches :: Array RestaurantId, othersLiked :: Array RestaurantId }
  | FailedToLoad ErrorMessage
  -- | Start again from the session in the url
  | Retry
  -- | Leave this session for a new one
  | NewSession
  | ToggleMatches
  | ShareInvite
  | LinkCopied
  | HideCopied

-- | Show a failed request to the user
failed :: ApiError -> Message
failed = case _ of
  NotFound e -> FailedToLoad e
  Failed e -> FailedToLoad e

orFail :: forall a. (a -> Message) -> Either ApiError a -> Message
orFail = either failed

-- | Show the next restaurant, or say there are none left
advance :: Swiper -> Tuple Model (Array (Aff (Maybe Message)))
advance swiper = case uncons swiper.upcoming of
  Nothing -> Active swiper NoneLeft /\ []
  Just { head, tail } -> Active swiper { upcoming = tail, photo = 0, summaryOpen = false } (Swiping head) /\ []

-- | Restaurants someone else liked first, otherwise keeping the order
likedFirst :: Array RestaurantId -> Array Restaurant -> Array Restaurant
likedFirst ids rs = filter liked rs <> filter (not <<< liked) rs
  where
  liked r = elem r.id ids

-- | Fetch the session's vibe. A failed check keeps what is shown and tries again later.
pollVibe :: SessionId -> Aff (Maybe Message)
pollVibe session = getVibe session >>= case _ of
  Right v -> pure $ Just (VibePolled v)
  Left _ -> delay (Milliseconds 3000.0) $> Just PollVibe

-- | Initial state of the app
init :: Model
init = Loading

-- | `update` is called to handle events
update :: Update Model Message
update model = case model, _ of
  _, FailedToLoad e -> ServerError e /\ []
  _, Retry -> model /\ [ liftEffect reload $> Nothing ]
  _, NewSession -> model /\ [ liftEffect startNewSession $> Nothing ]

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
  _, JoinSession session link ->
    QR { session, link, copied: false, vibe: { chips: [], locked: false } } /\ [ pollVibe session ]
  -- Keeps checking while in the lobby so everyone sees the same chips; stops once swiping
  QR lobby, VibePolled v -> QR lobby { vibe = v } /\ [ delay (Milliseconds 3000.0) $> Just PollVibe ]
  QR { session }, PollVibe -> model /\ [ pollVibe session ]
  QR lobby, VibeSaved v -> QR lobby { vibe = v } /\ []
  QR lobby, ToggleChip id | not lobby.vibe.locked ->
    let
      chosen = if elem id lobby.vibe.chips then filter (_ /= id) lobby.vibe.chips else snoc lobby.vibe.chips id
    in
      QR lobby { vibe = lobby.vibe { chips = chosen } }
        /\ [ setVibe lobby.session chosen <#> either (const Nothing) (Just <<< VibeSaved) ]
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
        Right loc -> do
          returning <- liftEffect $ rememberedUser session
          joined <- joinSession session returning
          case joined of
            Right u -> liftEffect (rememberUser session u) $> Just (Started u loc)
            Left e -> pure $ Just $ failed e
    ]
  _, Started user loc ->
    Active { user, loc, radius: 0.0, maxRadius: 0.0, upcoming: [], photo: 0, summaryOpen: false, othersLiked: [], liked: [], matches: [], showMatches: false, vibe: [] } Fetching
      /\ [ Just <<< orFail Loaded <$> getRestaurants user loc Nothing ]
  Active swiper Fetching, Loaded found ->
    advance swiper
      { upcoming = likedFirst found.othersLiked found.restaurants
      , othersLiked = found.othersLiked
      , vibe = found.vibe
      , radius = found.radius
      , maxRadius = found.maxRadius
      }
  Active swiper NoneLeft, SearchFurther | swiper.radius < swiper.maxRadius ->
    Active swiper Fetching /\ [ Just <<< orFail Loaded <$> getRestaurants swiper.user swiper.loc (Just swiper.radius) ]

  Active swiper (Swiping r), Like -> model /\
    [ do
        liftEffect $ flyOut Swipe.Right
        decide swiper.user r.id true <#> Just <<< either failed \d -> Decided { id: r.id, liked: true, matches: d.matches, othersLiked: d.othersLiked }
    ]
  Active swiper (Swiping r), Dislike -> model /\
    [ do
        liftEffect $ flyOut Swipe.Left
        decide swiper.user r.id false <#> Just <<< either failed \d -> Decided { id: r.id, liked: false, matches: d.matches, othersLiked: d.othersLiked }
    ]
  -- Ignored if it's late and the card has already gone, so a doubled response can't skip one
  Active swiper (Swiping r), Decided d | r.id == d.id ->
    let
      liked = if d.liked then cons r swiper.liked else swiper.liked
      new = filter (\l -> elem l.id d.matches && not (elem l.id (_.id <$> swiper.matches))) liked
      next = swiper { upcoming = likedFirst d.othersLiked swiper.upcoming, othersLiked = d.othersLiked, liked = liked, matches = new <> swiper.matches }
    in
      -- Celebrate one, newest first; any others are in the list of matches
      case head new of
        Just m -> Active next (Matched m) /\ [ liftEffect vibrate $> Nothing ]
        Nothing -> advance next
  Active swiper (Swiping r), NextPhoto ->
    Active swiper { photo = min (swiper.photo + 1) (max 0 (length r.photos - 1)) } (Swiping r) /\ []
  Active swiper (Swiping r), ToggleSummary ->
    Active swiper { summaryOpen = not swiper.summaryOpen } (Swiping r) /\ []
  Active swiper (Swiping r), PreviousPhoto ->
    Active swiper { photo = max 0 (swiper.photo - 1) } (Swiping r) /\ []
  Active _ (Swiping _), KeyPressed "ArrowRight" -> update model Like
  Active _ (Swiping _), KeyPressed "ArrowLeft" -> update model Dislike

  Active swiper (Matched _), Continue -> advance swiper

  Active swiper screen, ToggleMatches ->
    Active swiper { showMatches = not swiper.showMatches } screen /\ []

  -- Anything else is a stale message, e.g. from a double click
  _, _ -> model /\ []
