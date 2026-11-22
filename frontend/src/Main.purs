module Main (main) where

import Prelude

import Effect (Effect)
import Flame as F
import Flame.Subscription.Document as FSD
import Flame.Subscription.Window as FSW
import Tender.App (Message(..), init, update)
import Tender.Swipe (listen, onSwipe)
import Tender.View (view)
import Web.DOM.ParentNode (QuerySelector(..))

-- | Mount the application on the given selector
main :: Effect Unit
main = do
  listen
  F.mount_ (QuerySelector "body")
    { model: init
    , subscribe: [ FSW.onLoad DetermineSession, FSD.onKeydown KeyPressed ] <> onSwipe { like: Like, nope: Dislike, previous: PreviousPhoto, next: NextPhoto }
    , update
    , view
    }
