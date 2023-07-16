{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module User (User (..), Session (..), ToField, ToField) where

import Data.UUID (UUID, fromText, toText)
import Database.SQLite.Simple (
    FromRow,
    ToRow,
    field,
    fromRow,
    toRow,
 )
import Database.SQLite.Simple.FromField (FromField (fromField))
import Database.SQLite.Simple.ToField (ToField (toField))
import GHC.Generics (Generic)

type Session = UUID

instance ToRow Session where
    toRow s = [toField $ toText s]

instance ToField UUID where
    toField s = toField $ toText s

instance {-# OVERLAPS #-} FromField (Maybe UUID) where
    fromField s = do
        fromText <$> fromField s

data User = User
    { userId :: UUID
    , session :: Session
    }
    deriving (Generic, Show)

instance FromRow (Maybe User) where
    fromRow = do
        f1 :: Maybe UUID <- field
        f2 :: Maybe Session <- field
        pure (User <$> f1 <*> f2)
