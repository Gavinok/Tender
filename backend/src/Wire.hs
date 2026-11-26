-- | The JSON the API reads and writes. Handlers use these types, and the
-- | contract tests encode and decode the same ones, so no shape is written
-- | out twice.
module Wire (
    -- * Requests
    RestaurantsRequest (..),
    VibeRequest (..),
    PlaceRequest (..),
    PresenceRequest (..),
    DecisionRequest (..),

    -- * Responses
    RestaurantsResponse (..),
    DecisionResponse (..),
    PresenceResponse (..),
    Lobby (..),
    ErrorResponse (..),
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.:?), (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import Presence (Person)
import Restaurant (Loc, Restaurant, RestaurantId)
import User qualified as U
import Vibe (Chip, Place, chipId)

-- | POST /api/restaurants
data RestaurantsRequest = RestaurantsRequest
    { searcher :: !U.UserId
    , loc :: !(Maybe Loc)
    -- ^ Nothing when the browser couldn't find the person; the session's is used
    , widenFrom :: !(Maybe Double)
    -- ^ Set by "search further": the radius the person was shown, to double
    }
    deriving stock (Eq, Generic, Show)

instance FromJSON RestaurantsRequest where
    parseJSON = withObject "RestaurantsRequest" $ \o ->
        RestaurantsRequest <$> o .: "user" <*> o .:? "loc" <*> o .:? "widenFrom"

-- | POST /api/vibe/:session, chip ids
newtype VibeRequest = VibeRequest [Text]
    deriving stock (Eq, Show)

instance FromJSON VibeRequest where
    parseJSON = withObject "VibeRequest" $ \o -> VibeRequest <$> o .: "chips"

-- | POST /api/place/:session: where they're eating, or null for their own locations
newtype PlaceRequest = PlaceRequest (Maybe Place)
    deriving stock (Eq, Show)

instance FromJSON PlaceRequest where
    parseJSON = withObject "PlaceRequest" $ \o -> PlaceRequest <$> o .:? "place"

-- | POST /api/presence
newtype PresenceRequest = PresenceRequest U.UserId
    deriving stock (Eq, Show)

instance FromJSON PresenceRequest where
    parseJSON = withObject "PresenceRequest" $ \o -> PresenceRequest <$> o .: "user"

-- | POST /api/decision
data DecisionRequest = DecisionRequest
    { user :: !U.UserId
    , restaurant :: !RestaurantId
    , liked :: !Bool
    }
    deriving stock (Eq, Generic, Show)

instance FromJSON DecisionRequest

-- | POST /api/restaurants
data RestaurantsResponse = RestaurantsResponse
    { rrRestaurants :: [Restaurant]
    , rrOthersLiked :: [RestaurantId]
    , rrVibe :: [Chip]
    , rrRadius :: Double
    , rrMaxRadius :: Double
    }

instance ToJSON RestaurantsResponse where
    toJSON r =
        object
            [ "restaurants" .= rrRestaurants r
            , "othersLiked" .= rrOthersLiked r
            , "vibe" .= map chipId (rrVibe r)
            , "radius" .= rrRadius r
            , "maxRadius" .= rrMaxRadius r
            ]

-- | POST /api/decision: the user's matches, and what others liked that they haven't decided on
data DecisionResponse = DecisionResponse
    { drMatches :: [RestaurantId]
    , drOthersLiked :: [RestaurantId]
    }

instance ToJSON DecisionResponse where
    toJSON r = object ["matches" .= drMatches r, "othersLiked" .= drOthersLiked r]

-- | POST /api/presence: everyone in the session, and the user's matches
data PresenceResponse = PresenceResponse
    { prPeople :: [Person]
    , prMatches :: [RestaurantId]
    }

instance ToJSON PresenceResponse where
    toJSON r = object ["people" .= prPeople r, "matches" .= prMatches r]

{- | GET and POST /api/vibe/:session and POST /api/place/:session: the
lobby's shared choices, and whether someone has started swiping and
locked them
-}
data Lobby = Lobby
    { lobbyChips :: [Chip]
    , lobbyLocked :: Bool
    , lobbyPlace :: Maybe Place
    }

instance ToJSON Lobby where
    toJSON l = object ["chips" .= map chipId (lobbyChips l), "locked" .= lobbyLocked l, "place" .= lobbyPlace l]

-- | Any error, with a message the frontend can show
newtype ErrorResponse = ErrorResponse Text

instance ToJSON ErrorResponse where
    toJSON (ErrorResponse msg) = object ["error" .= msg]
