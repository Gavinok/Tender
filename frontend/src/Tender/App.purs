module Tender.App
  ( Message(..)
  , Model(..)
  , Screen(..)
  , locationText
  , Lobby
  , Picker
  , Swiper
  , Decision
  , Toast
  , init
  , update
  ) where

import Prelude

import Data.Monoid (guard)

import Data.Array (cons, drop, elem, filter, head, length, null, snoc, uncons)
import Data.Bifunctor (lmap)
import Data.String (trim)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Effect.Aff (Aff, Milliseconds(..), attempt, delay)
import Effect.Class (liftEffect)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Flame (Update)
import Tender.Api (ApiError(..), PresenceResponse, createSession, decide, findPlaces, getRestaurants, getVibe, heartbeat, joinSession, setCraving, setPlace, setVibe)
import Data.Int (toNumber)
import Tender.Vibe (Chip, Vibe)
import Tender.Swipe (flyOut)
import Tender.Swipe as Swipe
import Tender.Browser (LocationProblem(..), Shared(..), confirmLeave, existingSession, leaveField, geolocation, reload, rememberSession, rememberUser, rememberedUser, shareLink, shareOrCopy, startNewSession, tabHidden, vibrate)
import Tender.Types (ErrorMessage, Found, Loc, Person, Place, Restaurant, RestaurantId, SessionId, Url, UserId)

-- | What we know once the user has joined a session
type Swiper =
  { session :: SessionId
  , user :: UserId
  -- | Nothing when the browser couldn't find the user, who then gets the
  -- | session's location
  , loc :: Maybe Loc
  -- | How far the session searches, in metres, and how far it can go
  , radius :: Number
  , maxRadius :: Number
  -- | Whether a wider search is already on its way
  , widening :: Boolean
  -- | Restaurants still to show, fetched once when swiping starts
  , upcoming :: Array Restaurant
  -- | Which of the current card's photos is showing
  , photo :: Int
  -- | Whether the current card's details sheet is open
  , detailsOpen :: Boolean
  -- | Restaurants someone else in the session liked, shown first and badged
  , othersLiked :: Array RestaurantId
  -- | Restaurants this person liked, newest first, to show when one becomes a match
  , liked :: Array Restaurant
  , matches :: Array Restaurant
  -- | Everyone in the session, as of the last heartbeat
  , people :: Array Person
  -- | Seconds until the next heartbeat, as the backend last said
  , heartbeatIn :: Int
  -- | Whether the list of matches is open
  , showMatches :: Boolean
  -- | Whether the sheet with who's here, the vibe and the invite is open
  , showSession :: Boolean
  -- | Show that the invite link was just copied
  , copied :: Boolean
  -- | Whether they closed the warning that their location wasn't found
  , warningDismissed :: Boolean
  -- | A match after the first, mentioned over the cards until the next swipe
  , toast :: Maybe Toast
  -- | Swipes the server hasn't heard yet, oldest first, sent again in the background
  , unsent :: Array Decision
  -- | The session's vibe chips
  , vibe :: Array Chip
  -- | What the group typed it's craving
  , craving :: Maybe String
  }

-- | A match mentioned over the cards. `leaving` while it slides away.
type Toast = { match :: Restaurant, leaving :: Boolean }

-- | A swipe
type Decision = { id :: RestaurantId, liked :: Boolean }

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
  -- | Open while someone is choosing where the group is eating
  , picker :: Maybe Picker
  -- | Whether the QR code is shown full screen, for scanning across a table
  , bigQR :: Boolean
  -- | Someone else just changed where the group is eating, so it's pointed out
  , placeChanged :: Boolean
  -- | Whether the vibe box is open, showing the chips as suggestions
  , vibeOpen :: Boolean
  -- | The craving as it's being typed, saved on Enter, Done or leaving the box
  , draft :: String
  }

-- | Looking up where the group is eating. `results` is Nothing until the
-- | first lookup comes back.
type Picker =
  { query :: String
  , results :: Maybe (Array Found)
  , searching :: Boolean
  , problem :: Maybe ErrorMessage
  }

-- | The model represents the state of the app
data Model
  = Loading
  -- | Waiting to start, with the link others can use to join
  | QR Lobby
  | Active Swiper Screen
  | ServerError ErrorMessage
  -- | Joined, but there's no location to search from: the user's couldn't be
  -- | found and nobody else in the session has one yet
  | NeedLocation { session :: SessionId, user :: UserId, message :: ErrorMessage, picker :: Maybe Picker }

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
  -- | Joined, with the user's location or why it couldn't be found
  | Started SessionId UserId (Either LocationProblem Loc)
  -- | The search had no location to use, with why the user's couldn't be
  -- | found and what the server said
  | LocationFailed SessionId UserId LocationProblem ErrorMessage
  -- | Ask for the user's location again, staying in the session
  | RetryLocation
  -- | Choosing where the group is eating, instead of using locations
  | OpenPicker
  | ClosePicker
  | PlaceQuery String
  | FindPlaces
  | PlacesFound (Either ErrorMessage (Array Found))
  | ChoosePlace Found
  -- | Back to everyone's own locations
  | ClearPlace
  | Like
  | Dislike
  -- | A swipe couldn't be sent, so it's kept to try again
  | NotSent Decision
  | RetryUnsent
  -- | The oldest unsent swipe got through
  | Resent
  -- | A key pressed anywhere on the page
  | KeyPressed String
  -- | Skip past the current match and keep swiping
  | Continue
  -- | The restaurants in the new ring after searching further while swiping
  | Widened { restaurants :: Array Restaurant, othersLiked :: Array RestaurantId, vibe :: Array Chip, craving :: Maybe String, radius :: Number, maxRadius :: Number }
  | Loaded { restaurants :: Array Restaurant, othersLiked :: Array RestaurantId, vibe :: Array Chip, craving :: Maybe String, radius :: Number, maxRadius :: Number }
  | SearchFurther
  | NextPhoto
  | PreviousPhoto
  | ToggleDetails
  | ToggleChip String
  -- | A chip picked from the suggestions, which stands in for typing
  | PickChip String
  -- | The vibe box was tapped into
  | OpenVibe
  -- | Done with the vibe box
  | CloseVibe
  | DraftCraving String
  | SaveCraving
  -- | The vibe as saved after this person changed it
  | VibeSaved Vibe
  -- | The vibe as fetched by the lobby's regular check, which then schedules the next one
  | VibePolled Vibe
  | PollVibe
  -- | The decision on this restaurant is saved. Brings the session's matches
  -- | this person is part of, including ones others completed since.
  | Decided { id :: RestaurantId, liked :: Boolean, matches :: Array RestaurantId, othersLiked :: Array RestaurantId, people :: Array Person }
  -- | Time to tell the server the user is still here
  | Heartbeat
  -- | Who is in the session and the user's matches, or Nothing if the
  -- | heartbeat failed
  | HeardBack (Maybe PresenceResponse)
  | FailedToLoad ErrorMessage
  -- | Start again from the session in the url
  | Retry
  -- | Leave this session for a new one
  | NewSession
  | ToggleMatches
  | ToggleSession
  -- | Stop mentioning this match over the cards, if it's still the one shown
  | HideToast RestaurantId
  -- | The toast has slid away
  | ToastGone RestaurantId
  | ToggleQR
  -- | Stop pointing out that the place changed
  | PlaceSeen
  | DismissWarning
  | ShareInvite
  | LinkCopied
  | HideCopied

-- | A swipe that didn't get through is kept, unless the session is gone
swipeFailed :: Decision -> ApiError -> Message
swipeFailed d = case _ of
  Failed _ -> NotSent d
  other -> failed other

-- | Wait a few seconds, then send the unsent swipes again
retryLater :: Aff (Maybe Message)
retryLater = delay (Milliseconds 5000.0) $> Just RetryUnsent

-- | A typed craving, or Nothing for blank
tidy :: String -> Maybe String
tidy typed = if trim typed == "" then Nothing else Just (trim typed)

-- | Whether the vibe box has a craving typed that isn't saved yet
unsaved :: Lobby -> Boolean
unsaved lobby = lobby.vibeOpen && tidy lobby.draft /= lobby.vibe.craving

-- | Whether a sheet is covering the cards
sheetOpen :: Swiper -> Boolean
sheetOpen swiper = swiper.showMatches || swiper.showSession || swiper.detailsOpen

-- | Show a failed request to the user
failed :: ApiError -> Message
failed = FailedToLoad <<< errorText

newPicker :: Picker
newPicker = { query: "", results: Nothing, searching: false, problem: Nothing }

pickerOf :: Model -> Maybe Picker
pickerOf = case _ of
  QR lobby -> lobby.picker
  NeedLocation n -> n.picker
  _ -> Nothing

-- | Change the open picker, wherever it is
withPicker :: Model -> (Maybe Picker -> Maybe Picker) -> Model
withPicker model f = case model of
  QR lobby -> QR lobby { picker = f lobby.picker }
  NeedLocation n -> NeedLocation n { picker = f n.picker }
  _ -> model

toPlace :: Found -> Place
toPlace f = { name: f.name, latitude: f.latitude, longitude: f.longitude }

locOf :: Place -> Loc
locOf p = { latitude: p.latitude, longitude: p.longitude }

-- | What went wrong finding the user's location, and what they can do
locationText :: LocationProblem -> String
locationText = case _ of
  Blocked -> "Location is blocked for this site. Allow it from the icon at the left of the address bar, then try again."
  DeviceOff -> "Your browser allows Tender to see your location, but your device isn't sharing it with the browser. Turn on location for your browser in your device's settings."
  NotAllowed -> "Tender didn't get permission to see your location. Try again and allow it when your browser asks."
  Unavailable -> "Your device couldn't work out where you are."
  TimedOut -> "Finding your location took too long."
  Unsupported -> "This browser can't share your location with Tender."

errorText :: ApiError -> ErrorMessage
errorText = case _ of
  NotFound e -> e
  Failed e -> e

orFail :: forall a. (a -> Message) -> Either ApiError a -> Message
orFail = either failed

-- | Show the next restaurant, or say there are none left
advance :: Swiper -> Tuple Model (Array (Aff (Maybe Message)))
advance swiper = case uncons swiper.upcoming of
  Nothing -> Active swiper NoneLeft /\ []
  Just { head, tail } -> Active swiper { upcoming = tail, photo = 0, detailsOpen = false } (Swiping head) /\ []

-- | Mention a match over whatever screen follows
withToast :: Restaurant -> Tuple Model (Array (Aff (Maybe Message))) -> Model
withToast m (shown /\ _) = case shown of
  Active s screen -> Active s { toast = Just { match: m, leaving: false } } screen
  other -> other

-- | How long until the next heartbeat. More often while out of cards, where
-- | someone is waiting to hear about matches.
seconds :: Int -> Milliseconds
seconds n = Milliseconds (1000.0 * toNumber n)

-- | Restaurants the user liked that are in the matches and weren't before,
-- | newest first, with them added to the user's matches
newMatches :: Array RestaurantId -> Swiper -> { new :: Array Restaurant, swiper :: Swiper }
newMatches ids swiper =
  let
    new = filter (\l -> elem l.id ids && not (elem l.id (_.id <$> swiper.matches))) swiper.liked
  in
    { new, swiper: swiper { matches = new <> swiper.matches } }

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
  _, NewSession -> model /\
    [ liftEffect do
        leave <- confirmLeave
        when leave startNewSession
        pure Nothing
    ]

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
    QR { session, link, copied: false, vibe: { chips: [], choices: [], locked: false, place: Nothing, craving: Nothing }, picker: Nothing, bigQR: false, placeChanged: false, vibeOpen: false, draft: "" } /\ [ pollVibe session ]
  -- Keeps checking while in the lobby so everyone sees the same chips; stops once swiping
  -- A place someone else changed is pointed out for a few seconds. Not on
  -- the first check, which only brings what the lobby already had.
  QR lobby, VibePolled v ->
    let
      changed = not (null lobby.vibe.choices) && v.place /= lobby.vibe.place
    in
      QR lobby { vibe = v, placeChanged = changed || lobby.placeChanged }
        /\ ([ delay (Milliseconds 3000.0) $> Just PollVibe ] <> if changed then [ delay (Milliseconds 3000.0) $> Just PlaceSeen ] else [])
  QR lobby, PlaceSeen -> QR lobby { placeChanged = false } /\ []
  QR { session }, PollVibe -> model /\ [ pollVibe session ]
  QR lobby, VibeSaved v -> QR lobby { vibe = v } /\ []
  QR lobby, ToggleChip id | not lobby.vibe.locked ->
    let
      chosen = if elem id lobby.vibe.chips then filter (_ /= id) lobby.vibe.chips else snoc lobby.vibe.chips id
    in
      QR lobby { vibe = lobby.vibe { chips = chosen } }
        /\ [ setVibe lobby.session chosen <#> either (const Nothing) (Just <<< VibeSaved) ]
  -- The vibe box: tapping in shows the chips as suggestions, and picking
  -- one stands in for typing it, so what was typed goes. The craving is
  -- saved on Enter, Done or Start swiping, not on every keystroke, and
  -- the lobby's regular check leaves what's being typed alone.
  -- Just added to the box. The typed text was only filtering the
  -- suggestions, so it goes, and so does the keyboard: on iPhones tapping
  -- a button leaves the cursor in the box.
  QR lobby, PickChip id | not lobby.vibe.locked ->
    let
      picked /\ effects = update (QR lobby { draft = "" }) (ToggleChip id)
    in
      picked /\ (effects <> [ liftEffect (leaveField "vibe-box") $> Nothing ])
  QR lobby, OpenVibe | not lobby.vibe.locked && not lobby.vibeOpen ->
    QR lobby { vibeOpen = true, draft = fromMaybe "" lobby.vibe.craving } /\ []
  QR lobby, DraftCraving typed -> QR lobby { draft = typed } /\ []
  QR lobby, SaveCraving | unsaved lobby ->
    QR lobby { vibe = lobby.vibe { craving = tidy lobby.draft } }
      /\ [ setCraving lobby.session lobby.draft <#> either (const Nothing) (Just <<< VibeSaved) ]
  QR _, CloseVibe ->
    let
      saved /\ effects = update model SaveCraving
    in
      case saved of
        QR l -> QR l { vibeOpen = false } /\ (effects <> [ liftEffect (leaveField "vibe-box") $> Nothing ])
        other -> other /\ effects
  QR { link }, ShareInvite -> model /\
    [ attempt (shareOrCopy link) <#> case _ of
        Right Copied -> Just LinkCopied
        _ -> Nothing
    ]
  QR lobby, LinkCopied ->
    QR lobby { copied = true } /\ [ delay (Milliseconds 2000.0) $> Just HideCopied ]
  QR lobby, HideCopied -> QR lobby { copied = false } /\ []
  QR lobby, ToggleQR -> QR lobby { bigQR = not lobby.bigQR } /\ []
  -- Escape closes whatever is on top: the big QR code, the place lookup, then the vibe box
  QR lobby, KeyPressed "Escape" | lobby.bigQR -> update model ToggleQR
  QR lobby, KeyPressed "Escape" | isJust lobby.picker -> update model ClosePicker
  QR lobby, KeyPressed "Escape" | lobby.vibeOpen -> update model CloseVibe
  -- Inviting more people from the session sheet once swiping
  Active { session } _, ShareInvite -> model /\
    [ attempt (liftEffect (shareLink session) >>= shareOrCopy) <#> case _ of
        Right Copied -> Just LinkCopied
        _ -> Nothing
    ]
  Active swiper screen, LinkCopied ->
    Active swiper { copied = true } screen /\ [ delay (Milliseconds 2000.0) $> Just HideCopied ]
  Active swiper screen, HideCopied -> Active swiper { copied = false } screen /\ []

  -- Without a location the user can still join, and gets the session's
  -- With a place chosen, nobody needs their location
  -- A craving still being typed is saved first, so the search uses it
  QR lobby@{ session, vibe }, StartSwiping -> model /\
    [ do
        when (unsaved lobby) $ void $ setCraving session lobby.draft
        located <- maybe geolocation (pure <<< Right <<< locOf) vibe.place
        returning <- liftEffect $ rememberedUser session
        joined <- joinSession session returning
        case joined of
          Right u -> liftEffect (rememberUser session u) $> Just (Started session u located)
          Left e -> pure $ Just $ failed e
    ]
  _, Started session user located ->
    let
      loc = either (const Nothing) Just located
      -- Only reached when nobody else in the session has a location either
      noLocation problem e = LocationFailed session user problem (errorText e)
    in
      Active { session, user, loc, radius: 0.0, maxRadius: 0.0, widening: false, upcoming: [], photo: 0, detailsOpen: false, othersLiked: [], liked: [], matches: [], people: [], heartbeatIn: 10, showMatches: false, showSession: false, copied: false, toast: Nothing, unsent: [], warningDismissed: false, vibe: [], craving: Nothing } Fetching
        /\
          [ Just <<< either (either noLocation (const failed) located) Loaded <$> getRestaurants user loc Nothing
          -- Soon after the search, so the server knows what the user was shown
          , delay (Milliseconds 2000.0) $> Just Heartbeat
          ]
  _, LocationFailed session user problem serverSaid ->
    NeedLocation { session, user, message: locationText problem <> " " <> serverSaid, picker: Nothing } /\ []
  NeedLocation { session, user }, RetryLocation ->
    Loading /\ [ Just <<< Started session user <$> geolocation ]

  -- Choosing where the group is eating, from the lobby or when there's no location
  QR lobby, OpenPicker | not lobby.vibe.locked -> QR lobby { picker = Just newPicker } /\ []
  NeedLocation n, OpenPicker -> NeedLocation n { picker = Just newPicker } /\ []
  _, ClosePicker -> withPicker model (const Nothing) /\ []
  _, PlaceQuery q -> withPicker model (map _ { query = q }) /\ []
  _, FindPlaces -> case pickerOf model of
    -- The backend says if it's too short or long, as it checks anyway
    Just p | not p.searching ->
      withPicker model (map _ { searching = true, problem = Nothing })
        /\ [ Just <<< PlacesFound <<< lmap errorText <$> findPlaces (trim p.query) ]
    _ -> model /\ []
  _, PlacesFound found ->
    withPicker model (map \p -> either (\e -> p { searching = false, problem = Just e }) (\fs -> p { searching = false, results = Just fs }) found) /\ []
  QR lobby, ChoosePlace f | not lobby.vibe.locked ->
    let
      place = toPlace f
    in
      QR lobby { picker = Nothing, vibe = lobby.vibe { place = Just place } }
        /\ [ setPlace lobby.session (Just place) <#> either (const Nothing) (Just <<< VibeSaved) ]
  QR lobby, ClearPlace | not lobby.vibe.locked ->
    QR lobby { picker = Nothing, vibe = lobby.vibe { place = Nothing } }
      /\ [ setPlace lobby.session Nothing <#> either (const Nothing) (Just <<< VibeSaved) ]
  -- Nobody has a location yet, so the session isn't locked and this sticks
  NeedLocation { session, user }, ChoosePlace f ->
    let
      place = toPlace f
    in
      Loading /\ [ setPlace session (Just place) $> Just (Started session user (Right (locOf place))) ]
  Active swiper Fetching, Loaded found ->
    advance swiper
      { upcoming = likedFirst found.othersLiked found.restaurants
      , othersLiked = found.othersLiked
      , vibe = found.vibe
      , craving = found.craving
      , radius = found.radius
      , maxRadius = found.maxRadius
      }
  Active swiper screen, Widened found ->
    let
      grown = swiper
        { upcoming = swiper.upcoming <> likedFirst found.othersLiked found.restaurants
        , othersLiked = found.othersLiked <> swiper.othersLiked
        , radius = found.radius
        , maxRadius = found.maxRadius
        , widening = false
        }
    in
      case screen of
        NoneLeft -> advance grown
        _ -> Active grown screen /\ []
  Active swiper (Swiping r), SearchFurther | swiper.radius < swiper.maxRadius && not swiper.widening ->
    Active swiper { widening = true } (Swiping r) /\ [ Just <<< orFail Widened <$> getRestaurants swiper.user swiper.loc (Just swiper.radius) ]
  Active swiper NoneLeft, SearchFurther | swiper.radius < swiper.maxRadius ->
    Active swiper Fetching /\ [ Just <<< orFail Loaded <$> getRestaurants swiper.user swiper.loc (Just swiper.radius) ]

  Active swiper (Swiping r), Like -> model /\
    [ do
        liftEffect $ flyOut Swipe.Right
        decide swiper.user r.id true <#> Just <<< either (swipeFailed { id: r.id, liked: true }) \d -> Decided { id: r.id, liked: true, matches: d.matches, othersLiked: d.othersLiked, people: d.people }
    ]
  Active swiper (Swiping r), Dislike -> model /\
    [ do
        liftEffect $ flyOut Swipe.Left
        decide swiper.user r.id false <#> Just <<< either (swipeFailed { id: r.id, liked: false }) \d -> Decided { id: r.id, liked: false, matches: d.matches, othersLiked: d.othersLiked, people: d.people }
    ]
  -- Ignored if it's late and the card has already gone, so a doubled response can't skip one
  Active swiper (Swiping r), Decided d | r.id == d.id ->
    let
      liked = if d.liked then cons r swiper.liked else swiper.liked
      { new, swiper: next } = newMatches d.matches swiper { upcoming = likedFirst d.othersLiked swiper.upcoming, othersLiked = d.othersLiked, liked = liked, people = d.people }
    in
      -- Celebrate the first match, newest first; later ones get a toast so
      -- swiping isn't interrupted. Any others are in the list of matches.
      case head new of
        Just m
          | null swiper.matches -> Active next (Matched m) /\ [ liftEffect vibrate $> Nothing ]
          | otherwise -> withToast m (advance next) /\ [ liftEffect vibrate $> Nothing, delay (Milliseconds 5000.0) $> Just (HideToast m.id) ]
        Nothing -> advance next
  -- Late, but who's here and how far along is still news
  Active swiper screen, Decided d -> Active swiper { people = d.people } screen /\ []
  -- The card has already flown off, so carry on with the next and keep trying
  -- quietly. Only a vanished session is worth stopping for.
  Active swiper screen, NotSent d ->
    let
      queued = swiper { unsent = snoc swiper.unsent d }
      retry = guard (null swiper.unsent) [ retryLater ]
    in
      case screen of
        Swiping r | r.id == d.id ->
          let
            next = queued { liked = if d.liked then cons r queued.liked else queued.liked }
            shown /\ effects = advance next
          in
            shown /\ (effects <> retry)
        _ -> Active queued screen /\ retry
  Active swiper _, RetryUnsent -> case head swiper.unsent of
    Nothing -> model /\ []
    Just d -> model /\
      [ decide swiper.user d.id d.liked >>= either (const retryLater) (const (pure (Just Resent))) ]
  Active swiper screen, Resent ->
    Active swiper { unsent = drop 1 swiper.unsent } screen /\ if length swiper.unsent > 1 then [ pure (Just RetryUnsent) ] else []
  Active swiper (Swiping r), NextPhoto ->
    Active swiper { photo = min (swiper.photo + 1) (max 0 (length r.photos - 1)) } (Swiping r) /\ []
  Active swiper (Swiping r), ToggleDetails ->
    Active swiper { detailsOpen = not swiper.detailsOpen } (Swiping r) /\ []
  Active swiper (Swiping r), PreviousPhoto ->
    Active swiper { photo = max 0 (swiper.photo - 1) } (Swiping r) /\ []
  -- Escape closes whichever dialog is on top
  Active _ (Matched _), KeyPressed "Escape" -> update model Continue
  Active swiper _, KeyPressed "Escape" | swiper.showMatches -> update model ToggleMatches
  Active swiper _, KeyPressed "Escape" | swiper.showSession -> update model ToggleSession
  Active swiper (Swiping _), KeyPressed "Escape" | swiper.detailsOpen -> update model ToggleDetails
  -- Arrow keys swipe, but not from behind an open sheet
  Active swiper (Swiping _), KeyPressed "ArrowRight" | not (sheetOpen swiper) -> update model Like
  Active swiper (Swiping _), KeyPressed "ArrowLeft" | not (sheetOpen swiper) -> update model Dislike

  Active swiper (Matched _), Continue -> advance swiper

  -- One loop for the whole session, started with it. Skipped while the page
  -- is hidden, so a phone in a pocket shows as away; a failed heartbeat just
  -- waits for the next.
  Active swiper _, Heartbeat -> model /\
    [ do
        hidden <- liftEffect tabHidden
        if hidden then delay (seconds swiper.heartbeatIn) $> Just Heartbeat
        else Just <<< HeardBack <<< either (const Nothing) Just <$> heartbeat swiper.user
    ]
  -- Out of cards, new matches are celebrated here; while swiping, each
  -- decision brings them instead
  Active swiper screen, HeardBack heard ->
    let
      -- The backend says when to come back; after a failure, the last it said
      withPeople = maybe swiper (\h -> swiper { people = h.people, heartbeatIn = h.next }) heard
      { new, swiper: next } = case screen, heard of
        NoneLeft, Just h -> newMatches h.matches withPeople
        _, _ -> { new: [], swiper: withPeople }
      again = delay (seconds next.heartbeatIn) $> Just Heartbeat
    in
      case head new of
        Just m -> Active next (Matched m) /\ [ liftEffect vibrate $> Nothing, again ]
        Nothing -> Active next screen /\ [ again ]

  -- Slides away first, then goes; a newer toast is left alone
  Active swiper screen, HideToast id | map (\t -> t.match.id) swiper.toast == Just id ->
    Active swiper { toast = map _ { leaving = true } swiper.toast } screen /\ [ delay (Milliseconds 250.0) $> Just (ToastGone id) ]
  Active swiper screen, ToastGone id | map (\t -> t.match.id) swiper.toast == Just id ->
    Active swiper { toast = Nothing } screen /\ []
  Active swiper screen, ToggleSession ->
    Active swiper { showSession = not swiper.showSession } screen /\ []
  Active swiper screen, DismissWarning ->
    Active swiper { warningDismissed = true } screen /\ []

  Active swiper screen, ToggleMatches ->
    Active swiper { showMatches = not swiper.showMatches } screen /\ []

  -- Anything else is a stale message, e.g. from a double click
  _, _ -> model /\ []
