{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.User where

import           Data.Aeson
import qualified Data.Text                     as T
import qualified Core.Time                     as Time
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField


data User = User {
    userId :: Int,
    userFirstName :: T.Text,
    userLastName :: T.Text,
    userPicture :: Maybe Int,
    userCreatedOn :: Time.UTCTime,
    userIsAdmin :: Bool
} deriving Show

defaultUser = User { userId        = -1
                   , userFirstName = ""
                   , userLastName  = ""
                   , userPicture   = Nothing
                   , userCreatedOn = Time.defaultToday
                   , userIsAdmin   = False
                   }

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
    toRow User {..} =
        [ toField userId
        , toField userFirstName
        , toField userLastName
        , toField userPicture
        , toField userCreatedOn
        , toField userIsAdmin
        ]

instance ToJSON User where
    toJSON User {..} = object
        [ "id" .= userId
        , "first_name" .= userFirstName
        , "last_name" .= userLastName
        , "created_on" .= userCreatedOn
        , "is_admin" .= userIsAdmin
        ]

data UserRaw = UserRaw {
    userRawFirstName :: T.Text,
    userRawLastName :: T.Text,
    userRawIsAdmin :: Maybe Bool
} deriving (Show, Eq)

instance FromJSON UserRaw where
    parseJSON (Object v) =
        UserRaw
            <$> v
            .:  "first_name"
            <*> v
            .:  "last_name"
            <*> v
            .:! "is_admin"


