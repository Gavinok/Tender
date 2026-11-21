module Main (main) where

import Prelude

import Effect (Effect)
import Flame (QuerySelector(..), (:>))
import Flame as F
import Flame.Subscription.Window as FSW
import Tender.App (Message(..), init, update)
import Tender.View (view)

-- | Mount the application on the given selector
main :: Effect Unit
main =
  F.mount_ (QuerySelector "body")
    { init: init :> []
    , subscribe: [ FSW.onLoad DetermineSession ]
    , update
    , view
    }
