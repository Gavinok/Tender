{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Restaurant (Series (..), Hours (..), Business (..), Resturant (..), Loc (..), bid) where

import Data.Aeson (FromJSON, ToJSON, parseJSON, (.:), (.=))
import qualified Data.Aeson as A
import Data.Maybe (listToMaybe)
import Database.SQLite.Simple (
    FromRow (..),
    ResultError (ConversionFailed),
    SQLData (SQLInteger, SQLNull),
    ToRow (..),
    field,
 )
import Database.SQLite.Simple.FromField (FromField, fromField, returnError)
import qualified Database.SQLite.Simple.Internal as I
import Database.SQLite.Simple.Ok (Ok (Ok))
import Database.SQLite.Simple.ToField (ToField (toField))
import GHC.Generics (Generic)

newtype Hours = Hours
    { is_open_now :: Bool
    }
    deriving (Generic, Show)

instance FromJSON Hours
instance FromField [Hours] where
    fromField f@(I.Field (SQLInteger b) _) =
        if b == 0 || b == 1
            then Ok [Hours (b /= 0)]
            else returnError ConversionFailed f ("Hours must be 0 or 1, got " ++ show b)
    fromField (I.Field SQLNull _) = Ok [Hours False]
    fromField f = returnError ConversionFailed f "Hours must an SQLInteger"

data Business = Business
    { id :: String
    , url :: String
    , name :: String
    , rating :: Float
    , price :: Maybe String
    , hours :: [Hours]
    }
    deriving stock (Generic, Show)

bid :: Business -> String
bid (Business id_ _ _ _ _ _) = id_

instance FromJSON Business
deriving anyclass instance FromRow Business

newtype Series = Series [Business] deriving (Show)

instance FromJSON Series where
    parseJSON (A.Object o) = o .: "data" >>= (.: "search") >>= (.: "business") >>= fmap Series . A.parseJSON
    parseJSON x = fail $ "Unexpected json: " ++ show x

data Resturant = Resturant
    { bus :: Business
    , imageLink :: Maybe String
    , location :: Loc
    }
    deriving (Generic, Show)
instance ToJSON Resturant where
    toJSON (Resturant (Business ident _ rname rrating rprice _) imgLink _) =
        A.object
            [ "id" .= (ident :: String)
            , "name" .= (rname :: String)
            , "price" .= (rprice :: Maybe String)
            , "rating" .= (rrating :: Float)
            , "imageLink" .= (imgLink :: Maybe String)
            ]
instance FromRow Resturant where
    fromRow =
        Resturant
            <$> ( Business <$> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                )
                <*> field
                <*> (Loc <$> field <*> field)
instance ToRow Resturant where
    toRow (Resturant b i l) =
        [ toField $ bid b
        , toField $ url b
        , toField $ name b
        , toField $ rating b
        , toField $ price b
        , case listToMaybe $ hours b of
            Nothing -> SQLNull
            Just (Hours h) -> toField h
        , toField i
        , toField $ latitude l
        , toField $ longitude l
        ]

data Loc = Loc
    { latitude :: Float
    , longitude :: Float
    }
    deriving (Generic, Show)

instance FromJSON Loc
