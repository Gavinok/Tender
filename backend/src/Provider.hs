-- | A source of restaurants, and trying several in order
module Provider (Provider (..), searchWithFallback) where

import Data.Text (Text)
import Data.Text qualified as T
import Fetch (FetchError)
import Restaurant (Restaurant)
import Vibe (Search (..))

data Provider = Provider
    { providerName :: Text
    , searchNear :: Search -> IO (Either FetchError [Restaurant])
    -- ^ Sources that can't use the vibe search by location alone
    }

{- | Results from the first provider that finds anything, logging each one
that fails or comes back empty. Nothing when every provider failed, as
opposed to Just [] when they answered but found nothing.
-}
searchWithFallback :: (String -> IO ()) -> [Provider] -> Search -> IO (Maybe [Restaurant])
searchWithFallback warn providers l = go False providers
  where
    go answered [] = pure (if answered then Just [] else Nothing)
    go answered (p : rest) = do
        found <- searchNear p l
        case found of
            Right rs@(_ : _) -> pure (Just rs)
            Right [] -> do
                warn $ T.unpack (providerName p) ++ " found nothing near " ++ show l
                go True rest
            Left e -> do
                warn $ T.unpack (providerName p) ++ " search near " ++ show l ++ " failed: " ++ show e
                go answered rest
