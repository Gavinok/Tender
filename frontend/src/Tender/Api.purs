module Tender.Api
  ( ApiError(..)
  , createSession
  , decide
  , getRestaurants
  , heartbeat
  , getVibe
  , setVibe
  , setPlace
  , findPlaces
  , joinSession
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)
import Effect.Aff (Aff, Error, attempt, message)
import Effect.Class (liftEffect)
import Fetch (Method(..), Response, fetch)
import Tender.Browser (backendUrl, encodeQuery)
import Tender.Types (ErrorMessage, Found, Loc, Person, Place, Restaurant, RestaurantId, SessionId(..), UserId(..))
import Tender.Vibe (Vibe)
import Yoga.JSON (class ReadForeign, class WriteForeign)
import Yoga.JSON as JSON

data ApiError
  = NotFound ErrorMessage
  | Failed ErrorMessage

-- | Decode a 200 response, or use the message from an error response
decode
  :: forall a
   . ReadForeign a
  => Either Error Response
  -> Aff (Either ApiError a)
decode (Left e) = pure $ Left $ Failed $ "Could not reach the server: " <> message e
decode (Right { status, text }) = do
  body <- text
  pure case status of
    200 -> lmap (Failed <<< ("Unexpected response: " <> _) <<< show) (JSON.readJSON body)
    404 -> Left $ NotFound $ errorMessage body
    _ -> Left $ Failed $ errorMessage body
  where
  errorMessage body = case JSON.readJSON body of
    Right ({ error } :: { error :: String }) -> error
    Left _ -> "The server responded with " <> show status

get :: forall a. ReadForeign a => String -> Aff (Either ApiError a)
get path = do
  url <- liftEffect $ backendUrl path
  decode =<< attempt (fetch url { method: GET })

post :: forall body a. WriteForeign body => ReadForeign a => String -> body -> Aff (Either ApiError a)
post path body = do
  url <- liftEffect $ backendUrl path
  decode =<< attempt
    ( fetch url
        { method: POST
        , headers: { "Content-Type": "application/json" }
        , body: JSON.writeJSON body
        }
    )

createSession :: Aff (Either ApiError SessionId)
createSession = get "newsession"

-- | Say the user is still here. Answers with everyone in the session and
-- | the restaurants the user and someone else both liked.
heartbeat :: UserId -> Aff (Either ApiError { people :: Array Person, matches :: Array RestaurantId })
heartbeat user = post "presence" { user }

getVibe :: SessionId -> Aff (Either ApiError Vibe)
getVibe (SessionId s) = get ("vibe/" <> s)

-- | Set the session's chips. Ignored once someone has started swiping.
setVibe :: SessionId -> Array String -> Aff (Either ApiError Vibe)
setVibe (SessionId s) chosen = post ("vibe/" <> s) { chips: chosen }

-- | Set where the session is eating, or with Nothing go back to everyone's
-- | own locations. Ignored once someone has started swiping.
setPlace :: SessionId -> Maybe Place -> Aff (Either ApiError Vibe)
setPlace (SessionId s) place = post ("place/" <> s) { place }

-- | Places matching what the user typed, e.g. a neighbourhood or address
findPlaces :: String -> Aff (Either ApiError (Array Found))
findPlaces q = get ("places?q=" <> encodeQuery q)

-- | Join the session, keeping the given id if this browser was in it before
joinSession :: SessionId -> Maybe UserId -> Aff (Either ApiError UserId)
joinSession (SessionId s) returning =
  get ("addtosession/" <> s <> maybe "" (\(UserId u) -> "?user=" <> u) returning)

-- | Record whether the user liked the restaurant. Says which restaurants the
-- | user has matched on so far, and what others in the session have liked
-- | that this user hasn't decided on yet.
decide :: UserId -> RestaurantId -> Boolean -> Aff (Either ApiError { matches :: Array RestaurantId, othersLiked :: Array RestaurantId })
decide user restaurant liked = post "decision" { user, restaurant, liked }

-- | Restaurants for the user's session. The session searches from the first
-- | location anyone in it sent, so everyone gets the same restaurants. A user
-- | without a location sends none and gets the session's.
-- | With `widenFrom`, the session searches further first, doubling the radius
-- | in metres the person was shown.
getRestaurants
  :: UserId
  -> Maybe Loc
  -> Maybe Number
  -> Aff (Either ApiError { restaurants :: Array Restaurant, othersLiked :: Array RestaurantId, vibe :: Array String, radius :: Number, maxRadius :: Number })
getRestaurants user loc widenFrom = post "restaurants" { user, loc, widenFrom }
