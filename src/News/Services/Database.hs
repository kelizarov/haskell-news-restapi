module News.Services.Database where

import qualified Database.PostgreSQL.Simple.FromRow as PSQL
import qualified Database.PostgreSQL.Simple.ToField as PSQL
import qualified Database.PostgreSQL.Simple.ToRow as PSQL

import qualified News.Models.Persisted as M
import qualified News.Models.User as M

newtype PersistedUser =
  PersistedUser (M.Persisted M.User)
  deriving (Show, Eq)

instance PSQL.ToRow PersistedUser where
  toRow (PersistedUser M.Persisted {..}) =
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
    PersistedUser <$>
    (M.Persisted <$> (M.ID <$> PSQL.field) <*>
     (M.User <$> PSQL.field <*> PSQL.field <*> PSQL.field <*> PSQL.field <*>
      PSQL.field))

class Monad m =>
      Persists m
  where
  createUser :: M.User -> m (M.ID M.User)
  listUsers :: m [PersistedUser]
  getUserById :: Int -> m (Maybe PersistedUser)
