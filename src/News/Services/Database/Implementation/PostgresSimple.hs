{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module News.Services.Database.Implementation.PostgresSimple
  (
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Database.PostgreSQL.Simple.FromRow as PSQL
import qualified Database.PostgreSQL.Simple.ToField as PSQL
import qualified Database.PostgreSQL.Simple.ToRow as PSQL
import News.AppHandle (Application)
import qualified News.Models.Persisted as M
import qualified News.Models.User as M
import News.Services.Database.Query.User (PersistentUser(..))

instance PSQL.ToRow (M.Persisted M.User) where
  toRow M.Persisted {..} =
    let M.ID uId = getId
        M.User {..} = getObj
     in [ PSQL.toField uId,
          PSQL.toField uFirstName,
          PSQL.toField uLastName,
          PSQL.toField uAvatarPath,
          PSQL.toField uCreatedAt,
          PSQL.toField uIsAdmin
        ]

instance PSQL.FromRow (M.Persisted M.User) where
  fromRow =
    M.Persisted <$> (M.ID <$> PSQL.field)
      <*> ( M.User <$> PSQL.field <*> PSQL.field <*> PSQL.field <*> PSQL.field
              <*> PSQL.field
          )

instance PersistentUser Application where
  createUser firstName lastName isAdmin = do
    conn <- asks undefined
    let query =
          "INSERT INTO users (first_name, last_name, is_admin, created_on) \
          \VALUES (?, ?, ?, CURRENT_TIMESTAMP) RETURNING *;"
    (entity : _) <- liftIO $ PSQL.query conn query (firstName, lastName, isAdmin)
    pure entity
  getUserById userId = do
    conn <- asks undefined
    let query = "SELECT * FROM users WHERE id = ?;"
    res :: [(M.Persisted M.User)] <- liftIO $ PSQL.query conn query [userId]
    pure $ listToMaybe res
  getUsers mbPage mbPageSize = do
    conn <- asks undefined
    let query = "SELECT * FROM users OFFSET ? LIMIT ?;"
    users :: [(M.Persisted M.User)] <-
      liftIO $ PSQL.query conn query [mbPage, mbPageSize]
    pure users
  updateUser userId M.User {..} = undefined
