-- | Vibe chips a group picks in the lobby, and what each means for the search
module Vibe (Chip (..), Search (..), Place (..), Found (..), defaultRadius, maxRadius, allChips, chipId, chipLabel, chipFromId, queryTerms, chipFields, priceLevels, wantsOpenNow, craving, maxCraving) where

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
    , searchCraving :: Maybe Text
    -- ^ What the group typed they're in the mood for, e.g. "ramen"
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

{- | What the lobby shows. The frontend gets these with the ids, so a chip
is defined only here.
-}
chipLabel :: Chip -> Text
chipLabel = \case
    Cheap -> "💸 Cheap eats"
    Fancy -> "✨ Treat yourself"
    Patio -> "☀️ Patio"
    DateNight -> "🕯️ Date night"
    BigGroup -> "👯 Big group"
    Veggie -> "🥦 Veggie"
    Drinks -> "🍸 Drinks"
    Brunch -> "🥞 Brunch"
    LateNight -> "🌙 Late night"
    OpenNow -> "🕐 Open now"

-- | As the frontend shows it: the id it sends back, and the label
instance ToJSON Chip where
    toJSON c = object ["id" .= chipId c, "label" .= chipLabel c]

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

-- | The longest craving kept, in characters
maxCraving :: Int
maxCraving = 60

{- | What someone typed they're in the mood for, with spacing tidied, or
Nothing for blank. Left when it's too long.
-}
craving :: Text -> Either Text (Maybe Text)
craving t
    | T.length tidied > maxCraving = Left ("Keep it under " <> T.pack (show maxCraving) <> " characters")
    | T.null tidied = Right Nothing
    | otherwise = Right (Just tidied)
  where
    tidied = T.unwords (T.words t)

-- | Words for a text search, the craving first, e.g. "ramen patio cocktails restaurant"
queryTerms :: Maybe Text -> [Chip] -> Text
queryTerms wanted chips = T.unwords (maybe [] pure wanted <> mapMaybe term chips <> ["restaurant"])
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

-- | The attributes Google can confirm for a chip, as it names them in a place
chipFields :: Chip -> [Text]
chipFields = \case
    Patio -> ["outdoorSeating"]
    BigGroup -> ["goodForGroups"]
    Veggie -> ["servesVegetarianFood"]
    Drinks -> ["servesCocktails", "servesBeer", "servesWine"]
    Brunch -> ["servesBrunch"]
    _ -> []

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
