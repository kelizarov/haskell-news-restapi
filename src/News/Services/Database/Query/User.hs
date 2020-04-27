module News.Services.Database.Query.User where

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

class PersistentUser m where
  getUserById :: Int -> m (Maybe (M.Persisted M.User))
  getUsers :: Maybe Int -> Maybe Int -> m [(M.Persisted M.User)]
  createUser :: T.Text -> T.Text -> Bool -> m (M.Persisted M.User)
  updateUser :: Int -> M.User -> m (M.Persisted M.User)

packUser :: M.Persisted M.User -> M.User
packUser M.Persisted {..} = getObj
