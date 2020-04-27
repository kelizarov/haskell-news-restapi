{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module News.Services.Database.Implementation.PostgresSimple
  (
  )
where

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
import News.Services.Database.Queries.User

instance PersistentUser Application where
  createUser :: T.Text -> T.Text -> Bool -> Application (M.Persisted M.User)
  createUser firstName lastName isAdmin = do
    conn <- asks undefined
    let query =
          "INSERT INTO users (first_name, last_name, is_admin, created_on) \
          \VALUES (?, ?, ?, CURRENT_TIMESTAMP) RETURNING *;"
    (entity : _) <- liftIO $ PSQL.query conn query (firstName, lastName, isAdmin)
    pure entity
  getUserById :: Int -> Application (Maybe (M.Persisted M.User))
  getUserById userId = do
    conn <- asks undefined
    let query = "SELECT * FROM users WHERE id = ?;"
    res :: [(M.Persisted M.User)] <- liftIO $ PSQL.query conn query [userId]
    pure $ listToMaybe res
  getUsers :: Maybe Int -> Maybe Int -> Application [M.Persisted M.User]
  getUsers mbPage mbPageSize = do
    conn <- asks undefined
    let query = "SELECT * FROM users OFFSET ? LIMIT ?;"
    users :: [(M.Persisted M.User)] <-
      liftIO $ PSQL.query conn query [mbPage, mbPageSize]
    pure users
  updateUser :: Int -> M.User -> Application (M.Persisted M.User)
  updateUser userId M.User {..} = undefined
