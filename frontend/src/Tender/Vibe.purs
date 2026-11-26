-- | Vibe chips the group can pick in the lobby. The ids match the backend's.
module Tender.Vibe (Chip, Vibe, chips, chipLabel) where

import Prelude

import Data.Array (find)
import Data.Maybe (Maybe, maybe)
import Tender.Types (Place)

type Chip = { id :: String, label :: String }

-- | The session's chips and where they're eating, and whether someone has
-- | started swiping, which locks them
type Vibe = { chips :: Array String, locked :: Boolean, place :: Maybe Place }

chips :: Array Chip
chips =
  [ { id: "cheap", label: "💸 Cheap eats" }
  , { id: "fancy", label: "✨ Treat yourself" }
  , { id: "patio", label: "☀️ Patio" }
  , { id: "date-night", label: "🕯️ Date night" }
  , { id: "big-group", label: "👯 Big group" }
  , { id: "veggie", label: "🥦 Veggie" }
  , { id: "drinks", label: "🍸 Drinks" }
  , { id: "brunch", label: "🥞 Brunch" }
  , { id: "late-night", label: "🌙 Late night" }
  , { id: "open-now", label: "🕐 Open now" }
  ]

chipLabel :: String -> String
chipLabel id = maybe id _.label (find (\c -> c.id == id) chips)
