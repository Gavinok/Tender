-- | Focus handling for dialogs, see Dialog.js
module Tender.Dialog (watch) where

import Prelude

import Effect (Effect)

-- | Keep focus inside any open modal dialog. Call once on startup.
foreign import watch :: Effect Unit
