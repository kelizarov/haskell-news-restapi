{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Models.User where

import Core.Database
import qualified Core.Time as Time
import Data.Aeson
import Data.Proxy
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PSQL
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

data User = User
  { userId :: Int
  , userFirstName :: T.Text
  , userLastName :: T.Text
  , userPicture :: Maybe Int
  , userCreatedOn :: Time.UTCTime
  , userIsAdmin :: Bool
  } deriving (Show)

defaultUser =
  User
    { userId = -1
    , userFirstName = ""
    , userLastName = ""
    , userPicture = Nothing
    , userCreatedOn = Time.defaultToday
    , userIsAdmin = False
    }

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
  toRow User {..} =
    [ toField userFirstName
    , toField userLastName
    , toField userPicture
    , toField userIsAdmin
    ]

instance ToJSON User where
  toJSON User {..} =
    object
      [ "id" .= userId
      , "first_name" .= userFirstName
      , "last_name" .= userLastName
      , "created_on" .= userCreatedOn
      , "is_admin" .= userIsAdmin
      ]

data UserRaw = UserRaw
  { userRawFirstName :: T.Text
  , userRawLastName :: T.Text
  , userRawIsAdmin :: Maybe Bool
  } deriving (Show, Eq)

instance ToRow UserRaw where
  toRow UserRaw {..} =
    [toField userRawFirstName, toField userRawLastName, toField userRawIsAdmin]

instance FromJSON UserRaw where
  parseJSON (Object v) =
    UserRaw <$> v .: "first_name" <*> v .: "last_name" <*> v .:! "is_admin"

instance Persistent User where
  tableName _ = "users"
  update conn id User {..} = do
    (obj:_) <- PSQL.query conn q (userFirstName, userLastName, userIsAdmin, id)
    pure obj
    where
      q =
        "UPDATE " <> tableName (Proxy :: Proxy User) <>
        "SET (first_name, last_name, is_admin) = (?, ?, ?) WHERE id = ? RETRURNING *;"
  insert conn User {..} = do
    (obj:_) <- PSQL.query conn q (userFirstName, userLastName, userIsAdmin)
    pure obj
    where
      q =
        "INSERT INTO " <> tableName (Proxy :: Proxy User) <>
        "(first_name, last_name, is_admin) VALUES (?, ?, ?) RETRURNING *;"
