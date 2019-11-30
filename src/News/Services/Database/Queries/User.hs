{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module News.Services.Database.Queries.User where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks)

import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Database.PostgreSQL.Simple.FromRow as PSQL
import qualified Database.PostgreSQL.Simple.ToField as PSQL
import qualified Database.PostgreSQL.Simple.ToRow as PSQL

import News.AppHandle

import qualified News.Models.Persisted as M
import qualified News.Models.User as M

type PersistedUser = M.Persisted M.User

instance PSQL.ToRow PersistedUser where
  toRow M.Persisted {..} =
    let M.ID uId = getId
        M.User {..} = getObj
     in [ PSQL.toField uId
        , PSQL.toField uFirstName
        , PSQL.toField uLastName
        , PSQL.toField uAvatarPath
        , PSQL.toField uCreatedAt
        , PSQL.toField uIsAdmin
        ]

instance PSQL.FromRow PersistedUser where
  fromRow =
    M.Persisted <$> (M.ID <$> PSQL.field) <*>
    (M.User <$> PSQL.field <*> PSQL.field <*> PSQL.field <*> PSQL.field <*>
     PSQL.field)

data QueryType a where
  ById :: Int -> QueryType (Maybe a)
  Paginated :: Maybe Int -> Maybe Int -> QueryType [a]

class PersistentUser m where
  getUserById :: Int -> m (Maybe PersistedUser)
  getUsers :: Maybe Int -> Maybe Int -> m [PersistedUser]
  createUser :: T.Text -> T.Text -> Bool -> m PersistedUser
  updateUser :: Int -> M.User -> m PersistedUser

packUser :: PersistedUser -> M.User
packUser M.Persisted {..} = getObj

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:_) = Just a

instance PersistentUser Application where
  createUser :: T.Text -> T.Text -> Bool -> Application PersistedUser
  createUser firstName lastName isAdmin = do
    conn <- asks ahConnection
    let query =
          "INSERT INTO users (first_name, last_name, is_admin, created_on) \
            \VALUES (?, ?, ?, CURRENT_TIMESTAMP) RETURNING *;"
    (entity:_) <- liftIO $ PSQL.query conn query (firstName, lastName, isAdmin)
    pure entity
  getUserById :: Int -> Application (Maybe PersistedUser)
  getUserById userId = do
    conn <- asks ahConnection
    let query = "SELECT * FROM users WHERE id = ?;"
    res :: [PersistedUser] <- liftIO $ PSQL.query conn query [userId]
    pure $ listToMaybe res
  getUsers :: Maybe Int -> Maybe Int -> Application [PersistedUser]
  getUsers mbPage mbPageSize = do
    conn <- asks ahConnection
    let query = "SELECT * FROM users OFFSET ? LIMIT ?;"
    users :: [PersistedUser] <-
      liftIO $ PSQL.query conn query [mbPage, mbPageSize]
    pure users
  updateUser :: Int -> M.User -> Application PersistedUser
  updateUser userId M.User {..} = undefined
