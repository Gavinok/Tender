module Tender.Api
  ( ApiError(..)
  , createSession
  , decide
  , getRestaurants
  , joinSession
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect.Aff (Aff, Error, attempt, message)
import Effect.Class (liftEffect)
import Fetch (Method(..), Response, fetch)
import Tender.Browser (backendUrl)
import Tender.Types (ErrorMessage, Loc, Restaurant, RestaurantId, SessionId(..), UserId)
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

joinSession :: SessionId -> Aff (Either ApiError UserId)
joinSession (SessionId s) = get ("addtosession/" <> s)

-- | Record whether the user liked the restaurant, and whether that made it a match
decide :: UserId -> RestaurantId -> Boolean -> Aff (Either ApiError Boolean)
decide user restaurant liked =
  map _.match <$> (post "decision" { user, restaurant, liked } :: Aff (Either ApiError { match :: Boolean }))

-- | Restaurants near the location, most popular first
getRestaurants :: Loc -> Aff (Either ApiError (Array Restaurant))
getRestaurants loc = post "restaurants" loc
