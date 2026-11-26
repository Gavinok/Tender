-- | The server's own messages. On Cloud Run they're JSON lines, which
-- Cloud Logging reads as entries with a severity that can be filtered on;
-- locally they're plain lines.
module Log (Severity (..), Logger, LogFormat (..), newLogger) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Text (Text)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

data Severity = Info | Warning | Error
    deriving stock (Eq, Show)

-- | Log a message at a severity
type Logger = Severity -> String -> IO ()

data LogFormat = Plain | Json
    deriving stock (Eq, Show)

newLogger :: LogFormat -> Logger
newLogger Plain severity msg = hPutStrLn stderr (prefix severity ++ msg)
  where
    prefix Info = ""
    prefix s = show s ++ ": "
newLogger Json severity msg = do
    -- Cloud Logging reads these two fields of a JSON line on stdout
    L8.putStrLn (encode (object ["severity" .= name severity, "message" .= msg]))
    hFlush stdout
  where
    name :: Severity -> Text
    name Info = "INFO"
    name Warning = "WARNING"
    name Error = "ERROR"
