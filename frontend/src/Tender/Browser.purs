module Tender.Browser
  ( Shared(..)
  , backendUrl
  , existingSession
  , geolocation
  , rememberSession
  , reload
  , vibrate
  , sessionFromPath
  , shareLink
  , shareOrCopy
  ) where

import Prelude

import Data.Array (filter, last)
import Data.Maybe (Maybe)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Tender.Types (Loc, SessionId(..), Url)
import Web.HTML (window)
import Web.HTML.Location (origin, pathname)
import Web.HTML.Window (location)

foreign import _geolocation :: EffectFnAff Loc

-- | The backend is reached through the same origin under /api, so it works
-- | behind a tunnel or reverse proxy. In development parcel forwards /api to
-- | the backend on port 5001 (see .proxyrc.json).
backendUrl :: String -> Effect Url
backendUrl path = do
  o <- origin =<< location =<< window
  pure $ o <> "/api/" <> path

-- | Ask the browser for the user's location. Fails if they refuse.
geolocation :: Aff Loc
geolocation = fromEffectFnAff _geolocation

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
