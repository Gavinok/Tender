module Tender.Browser
  ( Shared(..)
  , backendUrl
  , existingSession
  , geolocation
  , LocationProblem(..)
  , rememberSession
  , rememberUser
  , rememberedUser
  , reload
  , vibrate
  , sessionFromPath
  , shareLink
  , shareOrCopy
  , confirmLeave
  , startNewSession
  , tabHidden
  , encodeQuery
  ) where

import Prelude

import Data.Array (filter, last)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Tender.Types (Loc, SessionId(..), Url, UserId(..))
import Web.HTML (window)
import Web.HTML.Location (assign, origin, pathname)
import Web.HTML.Window (location)

-- | Why the browser couldn't give the user's location
data LocationProblem
  -- | The site's location permission is blocked
  = Blocked
  -- | The site is allowed, but the device or its settings refused
  | DeviceOff
  -- | The prompt was dismissed, or the browser refused without asking
  | NotAllowed
  | Unavailable
  | TimedOut
  -- | No geolocation at all, e.g. on http
  | Unsupported

derive instance Eq LocationProblem

instance Show LocationProblem where
  show = case _ of
    Blocked -> "Blocked"
    DeviceOff -> "DeviceOff"
    NotAllowed -> "NotAllowed"
    Unavailable -> "Unavailable"
    TimedOut -> "TimedOut"
    Unsupported -> "Unsupported"

foreign import _locate :: (Loc -> Either String Loc) -> (String -> Either String Loc) -> EffectFnAff (Either String Loc)

-- | The backend is reached through the same origin under /api, so it works
-- | behind a tunnel or reverse proxy. In development parcel forwards /api to
-- | the backend (see the serve target in frontend/Makefile).
backendUrl :: String -> Effect Url
backendUrl path = do
  o <- origin =<< location =<< window
  pure $ o <> "/api/" <> path

-- | Ask the browser for the user's location. Fails if they refuse.
geolocation :: Aff (Either LocationProblem Loc)
geolocation = lmap problem <$> fromEffectFnAff (_locate Right Left)
  where
  problem = case _ of
    "blocked" -> Blocked
    "device" -> DeviceOff
    "unavailable" -> Unavailable
    "timeout" -> TimedOut
    "unsupported" -> Unsupported
    _ -> NotAllowed

-- | Link for other people to join the session
shareLink :: SessionId -> Effect Url
shareLink (SessionId session) = do
  o <- origin =<< location =<< window
  pure $ o <> "/" <> session

-- | The session in the page url, if any
existingSession :: Effect (Maybe SessionId)
existingSession = sessionFromPath <$> (pathname =<< location =<< window)

-- | The last segment of a url path, e.g. `/<session>`
sessionFromPath :: String -> Maybe SessionId
sessionFromPath path = SessionId <$> last (filter (_ /= "") (split (Pattern "/") path))

-- | Open the site's root, which starts a new session. A full page load, so
-- | Back returns to the session the user was in.
startNewSession :: Effect Unit
startNewSession = assign "/" =<< location =<< window

-- | Whether the page is in a background tab or the phone's screen is off.
-- | Not web-html's visibilityState, which reads readyState in 4.1.1 and so
-- | is never hidden. Fixed in 4.1.2, once the package set has it.
-- | Ask whether to leave the session, in the browser's own dialog
foreign import confirmLeave :: Effect Boolean

foreign import tabHidden :: Effect Boolean

-- | Text made safe to put in a url's query string
foreign import encodeQuery :: String -> String

foreign import reload :: Effect Unit

foreign import vibrate :: Effect Unit

foreign import _replacePath :: String -> Effect Unit

-- | Put the session in the url so reloading the page rejoins it
rememberSession :: SessionId -> Effect Unit
rememberSession (SessionId s) = _replacePath ("/" <> s)

data Shared
  = Shared
  | Copied
  | Cancelled

foreign import _shareOrCopy :: Url -> EffectFnAff String

-- | Open the share sheet, or copy the url where there isn't one
shareOrCopy :: Url -> Aff Shared
shareOrCopy url = fromEffectFnAff (_shareOrCopy url) <#> case _ of
  "copied" -> Copied
  "cancelled" -> Cancelled
  _ -> Shared

foreign import _rememberedUser :: (String -> Maybe String) -> Maybe String -> String -> Effect (Maybe String)

foreign import _rememberUser :: String -> String -> Effect Unit

-- | The id this browser had in the session, so coming back doesn't make
-- | the same person a second user
rememberedUser :: SessionId -> Effect (Maybe UserId)
rememberedUser (SessionId s) = map UserId <$> _rememberedUser Just Nothing s

rememberUser :: SessionId -> UserId -> Effect Unit
rememberUser (SessionId s) (UserId u) = _rememberUser s u
