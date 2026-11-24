{-# LANGUAGE DeriveAnyClass #-}

module User (User (..), UserId (..), SessionId (..), newUserId, newSessionId, sessionFromText, userFromText) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Database.SQLite.Simple (FromRow)
import Database.SQLite.Simple.FromField (FieldParser, FromField (fromField), ResultError (ConversionFailed), returnError)
import Database.SQLite.Simple.ToField (ToField (toField))
import GHC.Generics (Generic)

newtype UserId = UserId UUID
    deriving stock (Eq, Show)
    deriving newtype (FromJSON, ToJSON)

newtype SessionId = SessionId UUID
    deriving stock (Eq, Show)
    deriving newtype (FromJSON, ToJSON)

newUserId :: IO UserId
newUserId = UserId <$> nextRandom

newSessionId :: IO SessionId
newSessionId = SessionId <$> nextRandom

sessionFromText :: Text -> Maybe SessionId
sessionFromText = fmap SessionId . UUID.fromText

userFromText :: Text -> Maybe UserId
userFromText = fmap UserId . UUID.fromText

-- Both ids are stored as their textual form
instance ToField UserId where
    toField (UserId u) = toField (UUID.toText u)

instance ToField SessionId where
    toField (SessionId u) = toField (UUID.toText u)

instance FromField UserId where
    fromField f = UserId <$> uuidField f

instance FromField SessionId where
    fromField f = SessionId <$> uuidField f

uuidField :: FieldParser UUID
uuidField f = do
    t <- fromField f
    case UUID.fromText t of
        Just u -> pure u
        Nothing -> returnError ConversionFailed f ("Invalid UUID: " ++ show t)

data User = User
    { userId :: !UserId
    , session :: !SessionId
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromRow)
