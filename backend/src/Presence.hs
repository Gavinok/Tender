-- | Who in a session is still swiping, who is done and who has gone away.
-- | Kept in memory: it's only worth anything for a few seconds, and the app
-- | runs as one instance. Pure functions take the time; the IO wrappers
-- | read the clock.
module Presence (
    Seen (..),
    Activity (..),
    Person (..),
    Presence,
    newPresence,
    seenNow,
    dealtNow,
    lookupAll,
    pruneStale,
    seen,
    dealt,
    activity,
    cardsLeft,
    people,
    awayAfter,
) where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Restaurant (RestaurantId)
import User (UserId)

-- | When someone was last heard from, and every restaurant they were shown
data Seen = Seen
    { lastSeen :: UTCTime
    , deck :: Maybe (Set.Set RestaurantId)
    -- ^ Nothing until their first search comes back
    }
    deriving stock (Eq, Show)

data Activity = Swiping | Done | Away
    deriving stock (Bounded, Enum, Eq, Show)

instance ToJSON Activity where
    toJSON = toJSON . name
      where
        name :: Activity -> Text
        name = \case
            Swiping -> "swiping"
            Done -> "done"
            Away -> "away"

{- | One person in the session as the others see them. No ids: they let
| whoever holds them act as that person.
-}
data Person = Person
    { you :: Bool
    , status :: Activity
    , swiped :: Int
    , left :: Maybe Int
    -- ^ Nothing until their first search comes back
    }
    deriving stock (Eq, Show)

instance ToJSON Person where
    toJSON p = object ["you" .= you p, "status" .= status p, "swiped" .= swiped p, "left" .= left p]

-- | Heartbeats come every 10 seconds, and stop while the page is hidden
awayAfter :: NominalDiffTime
awayAfter = 30

-- | Heard from at the given time
seen :: UTCTime -> UserId -> Map.Map UserId Seen -> Map.Map UserId Seen
seen now = Map.alter (Just . maybe (Seen now Nothing) (\s -> s{lastSeen = now}))

-- | Shown these restaurants at the given time, as well as any before
dealt :: UTCTime -> UserId -> [RestaurantId] -> Map.Map UserId Seen -> Map.Map UserId Seen
dealt now u ids = Map.alter (Just . Seen now . Just . Set.union (Set.fromList ids) . shown) u
  where
    shown = maybe Set.empty (fromMaybe Set.empty . deck)

-- | Restaurants shown but not yet decided on, once there has been a search
cardsLeft :: Set.Set RestaurantId -> Seen -> Maybe Int
cardsLeft decided s = Set.size . (`Set.difference` decided) <$> deck s

activity :: UTCTime -> Set.Set RestaurantId -> Maybe Seen -> Activity
activity _ _ Nothing = Away
activity now decided (Just s)
    | diffUTCTime now (lastSeen s) > awayAfter = Away
    | cardsLeft decided s == Just 0 = Done
    | otherwise = Swiping

{- | Everyone in the session, in the order they joined, each with what
they've decided on, as the given user sees them
-}
people :: UTCTime -> UserId -> [(UserId, Set.Set RestaurantId)] -> Map.Map UserId Seen -> [Person]
people now me members presence =
    [ Person (u == me) (activity now decided s) (Set.size decided) (s >>= cardsLeft decided)
    | (u, decided) <- members
    , let s = Map.lookup u presence
    ]

-- | Who has been heard from, kept between requests
newtype Presence = Presence (IORef (Map.Map UserId Seen))

newPresence :: IO Presence
newPresence = Presence <$> newIORef Map.empty

seenNow :: Presence -> UserId -> IO ()
seenNow (Presence ref) u = do
    now <- getCurrentTime
    atomicModifyIORef' ref (\m -> (seen now u m, ()))

dealtNow :: Presence -> UserId -> [RestaurantId] -> IO ()
dealtNow (Presence ref) u ids = do
    now <- getCurrentTime
    atomicModifyIORef' ref (\m -> (dealt now u ids m, ()))

-- | Everyone heard from, with the time to judge them by
lookupAll :: Presence -> IO (UTCTime, Map.Map UserId Seen)
lookupAll (Presence ref) = (,) <$> getCurrentTime <*> readIORef ref

-- | Forget people not heard from for the given time
pruneStale :: NominalDiffTime -> Presence -> IO ()
pruneStale for (Presence ref) = do
    now <- getCurrentTime
    atomicModifyIORef' ref (\m -> (Map.filter ((<= for) . diffUTCTime now . lastSeen) m, ()))
