-- | Counting how much of something each visitor (or the whole site) has
-- | used in a time window, for rate limits. Everything is in memory, which
-- | is enough since the app runs as one instance.
module Limits (
    Limit (..),
    Usage,
    spend,
    Counter,
    newCounter,
    trySpend,
) where

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)

-- | At most this many per window
data Limit = Limit
    { limitWindow :: NominalDiffTime
    , limitMax :: Int
    }
    deriving (Show)

-- | Per key, when its window started and how much has been used since
type Usage = Map.Map Text (UTCTime, Int)

{- | Spend some of a key's allowance at the given time, if there's enough
left. Windows are fixed: they reset a window's length after their first
use. Expired keys are dropped as they're passed, so the map doesn't grow
with every visitor ever seen.
-}
spend :: Limit -> UTCTime -> Text -> Int -> Usage -> (Bool, Usage)
spend (Limit window most) now key cost usage =
    let current = Map.filter fresh usage
        used = maybe 0 snd (Map.lookup key current)
        started = maybe now fst (Map.lookup key current)
     in if used + cost > most
            then (False, current)
            else (True, Map.insert key (started, used + cost) current)
  where
    fresh (started, _) = addUTCTime window started > now

-- | A limit and the usage counted against it
data Counter = Counter Limit (IORef Usage)

newCounter :: Limit -> IO Counter
newCounter l = Counter l <$> newIORef Map.empty

-- | Spend some of the key's allowance now, if there's enough left
trySpend :: Counter -> Text -> Int -> IO Bool
trySpend (Counter l ref) key cost = do
    now <- getCurrentTime
    atomicModifyIORef' ref (\usage -> let (ok, usage') = spend l now key cost usage in (usage', ok))
