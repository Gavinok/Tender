-- | Vibe chips a group picks in the lobby, and what each means for the search
module Vibe (Chip (..), Search (..), Place (..), Found (..), defaultRadius, maxRadius, allChips, chipId, chipFromId, queryTerms, priceLevels, wantsOpenNow) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Restaurant (Loc (..))

-- | Where the group is eating, when they've said rather than use their locations
data Place = Place
    { placeName :: Text
    , placeLoc :: Loc
    }
    deriving stock (Eq, Show)

instance ToJSON Place where
    toJSON (Place n (Loc lat lon)) = object ["name" .= n, "latitude" .= lat, "longitude" .= lon]

-- | The coordinates are checked like any location; the name is shown to the group
instance FromJSON Place where
    parseJSON v = do
        n <- withObject "Place" (.: "name") v
        if T.null (T.strip n) || T.length n > 200
            then fail "a place needs a name of at most 200 characters"
            else Place n <$> parseJSON v

-- | A place a lookup found, with its address to tell it apart from others
data Found = Found
    { foundPlace :: Place
    , foundAddress :: Text
    }
    deriving stock (Eq, Show)

instance ToJSON Found where
    toJSON (Found (Place n (Loc lat lon)) address) =
        object ["name" .= n, "address" .= address, "latitude" .= lat, "longitude" .= lon]

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
    deriving stock (Bounded, Enum, Eq, Show)

-- | Where to look, and the session's vibe
data Search = Search
    { searchLoc :: Loc
    , searchChips :: [Chip]
    , searchRadius :: Double
    -- ^ Metres around the location
    , searchInner :: Double
    {- ^ Metres already searched when searching further, so only the ring
    between this and the radius is new. 0 for a first search.
    -}
    }
    deriving stock (Show)

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
