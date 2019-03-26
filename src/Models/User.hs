{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

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
  { uId :: Int
  , uFirstName :: T.Text
  , uLastName :: T.Text
  , uPicture :: Maybe Int
  , uCreatedOn :: Time.UTCTime
  , uIsAdmin :: Bool
  } deriving (Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToJSON User where
  toJSON User {..} =
    object
      [ "id" .= uId
      , "first_name" .= uFirstName
      , "last_name" .= uLastName
      , "created_on" .= uCreatedOn
      , "is_admin" .= uIsAdmin
      ]

data UserRaw = UserRaw
  { urFirstName :: T.Text
  , urLastName :: T.Text
  , urIsAdmin :: Maybe Bool
  } deriving (Show, Eq)

instance ToRow UserRaw where
  toRow UserRaw {..} =
    [toField urFirstName, toField urLastName, toField urIsAdmin]

instance FromJSON UserRaw where
  parseJSON (Object v) =
    UserRaw <$> v .: "first_name" <*> v .: "last_name" <*> v .:! "is_admin"

data UserQuery a where
  UserById :: Int -> UserQuery (Maybe User)
  UserList :: (Int, Int) -> UserQuery [User]

class MonadUserRepo m where
  queryUser :: UserQuery a -> m a
  createUser :: UserRaw -> m User
  updateUser :: Int -> UserRaw -> m (Maybe User)

instance MonadUserRepo MonadHandler where
  queryUser (UserById id) = do
    conn <- asks hConnection
    res <- liftIO $ PSQL.query conn q [id]
    case res of
      [] -> pure Nothing
      (obj:_) -> pure $ Just obj
    where
      q = "SELECT * FROM users WHERE id = ?;"
  queryUser (UserList params) = do
    conn <- asks hConnection
    liftIO (PSQL.query conn q params :: IO [User])
    where
      q = "SELECT * FROM users OFFSET ? LIMIT ?;"
  createUser UserRaw {..} = do
    conn <- asks hConnection
    (obj:_) <-
      liftIO
        (PSQL.query conn q (urFirstName, urLastName, urIsAdmin) :: IO [User])
    pure obj
    where
      q =
        "INSERT INTO users (first_name, last_name, is_admin, created_on) \
        \VALUES (?, ?, ?, CURRENT_TIMESTAMP) RETURNING *;"
  updateUser id UserRaw {..} = do
    conn <- asks hConnection
    res <-
      liftIO
        (PSQL.query conn q (urFirstName, urLastName, urIsAdmin, id) :: IO [User])
    case res of
      [] -> pure Nothing
      (obj:_) -> pure $ Just obj
    where
      q =
        "UPDATE users SET (first_name, last_name, is_admin) = (?, ?, ?) \
        \WHERE id = ? RETURNING *;"
