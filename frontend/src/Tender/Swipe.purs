-- | Swiping the restaurant card, see Swipe.js
module Tender.Swipe (Direction(..), flyOut, listen, onSwipe) where

import Prelude

import Effect (Effect)
import Flame (Subscription)
import Flame.Subscription (onCustomEvent')
import Web.Event.Event (EventType(..))

data Direction
  = Right
  | Left

-- | Start following drags on the card. Call once on startup.
foreign import listen :: Effect Unit

foreign import _flyOut :: String -> Effect Unit

-- | Throw the card off the screen, for likes and nopes from the buttons too
flyOut :: Direction -> Effect Unit
flyOut = case _ of
  Right -> _flyOut "like"
  Left -> _flyOut "nope"

-- | Swipes, and taps on the left third or the rest of the card
onSwipe :: forall message. { like :: message, nope :: message, previous :: message, next :: message } -> Array (Subscription message)
onSwipe { like, nope, previous, next } =
  [ onCustomEvent' (EventType "swipe-like") like
  , onCustomEvent' (EventType "swipe-nope") nope
  , onCustomEvent' (EventType "card-tap-left") previous
  , onCustomEvent' (EventType "card-tap-right") next
  ]
