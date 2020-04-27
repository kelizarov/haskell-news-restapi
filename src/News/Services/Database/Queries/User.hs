{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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

data QueryType a where
  ById :: Int -> QueryType (Maybe a)
  Paginated :: Maybe Int -> Maybe Int -> QueryType [a]

class PersistentUser m where
  getUserById :: Int -> m (Maybe (M.Persisted M.User))
  getUsers :: Maybe Int -> Maybe Int -> m [(M.Persisted M.User)]
  createUser :: T.Text -> T.Text -> Bool -> m (M.Persisted M.User)
  updateUser :: Int -> M.User -> m (M.Persisted M.User)

packUser :: (M.Persisted M.User) -> M.User
packUser M.Persisted {..} = getObj

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a : _) = Just a
