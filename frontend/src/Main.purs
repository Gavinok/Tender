module Main (main) where

import Prelude

import Effect (Effect)
import Flame as F
import Flame.Subscription.Window as FSW
import Tender.App (Message(..), init, update)
import Tender.View (view)
import Web.DOM.ParentNode (QuerySelector(..))

-- | Mount the application on the given selector
main :: Effect Unit
main =
  F.mount_ (QuerySelector "body")
    { model: init
    , subscribe: [ FSW.onLoad DetermineSession ]
    , update
    , view
    }
