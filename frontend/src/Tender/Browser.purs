module Tender.Browser
  ( backendUrl
  , existingSession
  , geolocation
  , shareLink
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
import Web.HTML.Location (hostname, origin, pathname, protocol)
import Web.HTML.Window (location)

foreign import _geolocation :: EffectFnAff Loc

-- | The backend runs on port 5001 of the host that served the page
backendUrl :: String -> Effect Url
backendUrl path = do
  loc <- location =<< window
  proto <- protocol loc
  host <- hostname loc
  pure $ proto <> "//" <> host <> ":5001/" <> path

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
existingSession = do
  path <- pathname =<< location =<< window
  pure $ SessionId <$> last (filter (_ /= "") (split (Pattern "/") path))
