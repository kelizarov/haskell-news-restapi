{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module News.Services.Database.Queries.User where

import qualified Database.PostgreSQL.Simple    as PSQL
import qualified Database.PostgreSQL.Simple.FromRow
                                               as PSQL
import qualified Database.PostgreSQL.Simple.ToField
                                               as PSQL
import qualified Database.PostgreSQL.Simple.ToRow
                                               as PSQL

import qualified News.Models.Persisted            as M
import qualified News.Models.User              as M

newtype PersistedUser = PersistedUser (M.Persisted M.User) deriving (Show, Eq)

instance PSQL.ToRow PersistedUser where
  toRow (PersistedUser M.Persisted {..}) =
    let M.ID uId    = getId
        M.User {..} = getObj
    in  [ PSQL.toField uId
        , PSQL.toField uFirstName
        , PSQL.toField uLastName
        , PSQL.toField uAvatarPath
        , PSQL.toField uCreatedAt
        , PSQL.toField uIsAdmin
        ]

instance PSQL.FromRow PersistedUser where
  fromRow =
    PersistedUser
      <$> (   M.Persisted
          <$> (M.ID <$> PSQL.field)
          <*> (   M.User
              <$> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              )
          )

-- data QueryType a where
--   ById :: Int -> QueryType (Maybe a)
--   Paginated :: (Maybe Int, Maybe Int) -> QueryType [a]

-- class PersistentUser m where
--   queryUser :: QueryType a -> m a
--   createUser :: M.User -> m PersistedUser
--   updateUser :: Int -> M.User -> m PersistedUser

packUser :: PersistedUser -> M.User
packUser (PersistedUser M.Persisted {..}) = getObj

listToMaybe :: [a] -> Maybe a
listToMaybe []      = Nothing
listToMaybe (a : _) = Just a

createUser :: PSQL.Connection -> M.User -> IO (M.ID M.User)
createUser conn M.User {..} = do
  (PersistedUser entity : _) <- PSQL.query conn
                                           query
                                           (uFirstName, uLastName, uIsAdmin)
  pure $ M.getId entity
 where
  query
    = "INSERT INTO users (first_name, last_name, is_admin, created_on) \
        \VALUES (?, ?, ?, CURRENT_TIMESTAMP) RETURNING *;"

listUsers :: PSQL.Connection -> IO [PersistedUser]
listUsers conn = do
  users :: [PersistedUser] <- PSQL.query conn query ()
  pure users
  where query = "SELECT * FROM users OFFSET ? LIMIT ?;"

getUserById :: PSQL.Connection -> Int -> IO (Maybe PersistedUser)
getUserById conn userId = do
  res :: [PersistedUser] <- PSQL.query conn query [userId]
  pure (listToMaybe res)
  where query = "SELECT * FROM users WHERE id = ?;"
