-- | Vibe chips the group can pick in the lobby. The backend defines them
-- | (backend/src/Vibe.hs) and sends them with the lobby, so a chip is added
-- | in one place.
module Tender.Vibe (Chip, Vibe) where

import Data.Maybe (Maybe)
import Tender.Types (Place)

-- | The id sent back when it's picked, and what the lobby shows
type Chip = { id :: String, label :: String }

-- | The chips to pick from and the ones picked, what the group typed it's
-- | craving, where they're eating, and whether someone has started swiping,
-- | which locks them
type Vibe = { chips :: Array String, choices :: Array Chip, locked :: Boolean, place :: Maybe Place, craving :: Maybe String }
