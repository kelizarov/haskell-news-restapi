{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Models.User where

import Control.Monad.Reader
import Core.Database
import Core.Monad.Handler
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

class UserDB m where
  getUser :: Int -> m (Maybe User)
  createUser :: UserRaw -> m User
  listUser :: (Int, Int) -> m [User]
  updateUser :: Int -> User -> m (Maybe User)

instance UserDB MonadHandler where
  getUser id = do
    conn <- asks hConnection
    res <- liftIO $ PSQL.query conn q [id]
    case res of
      [] -> pure Nothing
      (obj:_) -> pure $ Just obj
    where
      q = "SELECT * FROM users WHERE id = ?;"
  createUser UserRaw {..} = do
    conn <- asks hConnection
    (obj:_) <-
      liftIO
        (PSQL.query conn q (userRawFirstName, userRawLastName, userRawIsAdmin) :: IO [User])
    pure obj
    where
      q =
        "INSERT INTO users (first_name, last_name, is_admin, created_on) VALUES (?, ?, ?, CURRENT_TIMESTAMP) RETURNING *;"
  listUser (offset, limit) = do
    conn <- asks hConnection
    liftIO $ PSQL.query conn q ()
    where
      q = "SELECT * FROM users;"
  updateUser id user = undefined
