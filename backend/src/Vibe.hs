{-# LANGUAGE LambdaCase #-}

-- | Vibe chips a group picks in the lobby, and what each means for the search
module Vibe (Chip (..), Search (..), defaultRadius, maxRadius, allChips, chipId, chipFromId, queryTerms, priceLevels, wantsOpenNow) where

import Data.Maybe (mapMaybe)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Restaurant (Loc)

data Chip
    = Cheap
    | Fancy
    | Patio
    | DateNight
    | BigGroup
    | Veggie
    | Drinks
    | Brunch
    | LateNight
    | OpenNow
    deriving (Bounded, Enum, Eq, Show)

-- | Where to look, and the session's vibe
data Search = Search
    { searchLoc :: Loc
    , searchChips :: [Chip]
    , -- | Metres around the location
      searchRadius :: Double
    , -- | Metres already searched when searching further, so only the ring
      -- between this and the radius is new. 0 for a first search.
      searchInner :: Double
    }
    deriving (Show)

-- | Where a session's search starts, in metres
defaultRadius :: Double
defaultRadius = 5000

-- | The furthest Google's nearby search reaches, in metres
maxRadius :: Double
maxRadius = 50000

allChips :: [Chip]
allChips = [minBound .. maxBound]

-- | The id the frontend uses for the chip
chipId :: Chip -> Text
chipId = \case
    Cheap -> "cheap"
    Fancy -> "fancy"
    Patio -> "patio"
    DateNight -> "date-night"
    BigGroup -> "big-group"
    Veggie -> "veggie"
    Drinks -> "drinks"
    Brunch -> "brunch"
    LateNight -> "late-night"
    OpenNow -> "open-now"

chipFromId :: Text -> Maybe Chip
chipFromId t = lookup t [(chipId c, c) | c <- allChips]

-- | Words for a text search, e.g. "patio cocktails restaurant"
queryTerms :: [Chip] -> Text
queryTerms chips = T.unwords (mapMaybe term chips <> ["restaurant"])
  where
    term = \case
        Patio -> Just "patio"
        DateNight -> Just "romantic"
        BigGroup -> Just "good for groups"
        Veggie -> Just "vegetarian"
        Drinks -> Just "cocktails"
        Brunch -> Just "brunch"
        LateNight -> Just "late night"
        _ -> Nothing

-- | Google price levels the chips allow; empty means any
priceLevels :: [Chip] -> [Text]
priceLevels chips = nub (concatMap levels chips)
  where
    levels = \case
        Cheap -> ["PRICE_LEVEL_INEXPENSIVE", "PRICE_LEVEL_MODERATE"]
        Fancy -> ["PRICE_LEVEL_EXPENSIVE", "PRICE_LEVEL_VERY_EXPENSIVE"]
        _ -> []

wantsOpenNow :: [Chip] -> Bool
wantsOpenNow = elem OpenNow
