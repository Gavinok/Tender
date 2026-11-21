-- | A source of restaurants, and trying several in order
module Provider (Provider (..), searchWithFallback) where

import Data.Text (Text)
import qualified Data.Text as T
import Fetch (FetchError)
import Restaurant (Loc, Restaurant)

data Provider = Provider
    { providerName :: Text
    , searchNear :: Loc -> IO (Either FetchError [Restaurant])
    }

{- | Results from the first provider that finds anything, logging each one
that fails or comes back empty. Nothing when every provider failed, as
opposed to Just [] when they answered but found nothing.
-}
searchWithFallback :: (String -> IO ()) -> [Provider] -> Loc -> IO (Maybe [Restaurant])
searchWithFallback logMsg providers l = go False providers
  where
    go answered [] = pure (if answered then Just [] else Nothing)
    go answered (p : rest) = do
        found <- searchNear p l
        case found of
            Right rs@(_ : _) -> pure (Just rs)
            Right [] -> do
                logMsg $ T.unpack (providerName p) ++ " found nothing near " ++ show l
                go True rest
            Left e -> do
                logMsg $ T.unpack (providerName p) ++ " search near " ++ show l ++ " failed: " ++ show e
                go answered rest
