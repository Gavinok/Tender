{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Restaurant (Category (..), Series (..), Hours (..), Business (..), Resturant (..), Loc (..), bid) where

import Data.Aeson (FromJSON, ToJSON, parseJSON, (.:), (.=))
import qualified Data.Aeson as A
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Database.SQLite.Simple (
    FromRow (..),
    ResultError (ConversionFailed),
    SQLData (SQLInteger, SQLNull, SQLText),
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

newtype Category = Category
    { title :: String
    } deriving (Generic, Show)
instance FromJSON Category
instance FromField [Category] where
    fromField (I.Field (SQLText b) _) = Ok [Category $ show b]
    fromField (I.Field SQLNull _) = Ok []
    fromField f = returnError ConversionFailed f "Category must a SQLText"

data Business = Business
    { id :: String
    , url :: String
    , name :: String
    , rating :: Float
    , price :: Maybe String
    , hours :: [Hours]
    , categories :: [Category]
    }
    deriving stock (Generic, Show)

bid :: Business -> String
bid (Business id_ _ _ _ _ _ _) = id_

instance FromJSON Business

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
    toJSON (Resturant (Business ident rurl rname rrating rprice _ cat) imgLink _) =
        A.object
            [ "id" .= (ident :: String)
            , "name" .= (rname :: String)
            , "url" .= (rurl :: String)
            , "price" .= (rprice :: Maybe String)
            , "rating" .= (rrating :: Float)
            , "imageLink" .= (imgLink :: Maybe String)
            , "category" .= (Just (intercalate ", " $ title <$> cat) :: Maybe String)
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
                    <*> field
                )
                <*> field
                <*> (Loc <$> field <*> field)

instance ToRow Resturant where
    toRow (Resturant b i l) =
        [
          toField $ bid b
        , toField $ url b
        , toField $ name b
        , toField $ rating b
        , toField $ price b
        , case listToMaybe $ hours b of
            Nothing -> SQLNull
            Just (Hours h) -> toField h
        , case categories b of
            [] -> SQLNull
            x ->  toField (intercalate ", " $ title <$> x :: String)
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
