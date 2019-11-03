module News.Models.User where

import qualified Data.Text                     as T
import qualified Data.Time                     as Time
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow

import           News.Models.Entity

data User = User
  { uFirstName :: T.Text
  , uLastName :: T.Text
  , uAvatarPath :: T.Text
  , uCreatedAt :: Time.UTCTime
  , uIsAdmin :: Bool
  } deriving (Show, Eq)

newtype PersistedUser = PersistedUser (Entity User) deriving (Show, Eq)

instance ToRow PersistedUser where
  toRow (PersistedUser Entity {..}) =
    let ID uId    = getId
        User {..} = getObj
    in  [ toField uId
        , toField uFirstName
        , toField uLastName
        , toField uAvatarPath
        , toField uCreatedAt
        , toField uIsAdmin
        ]

instance FromRow PersistedUser where
  fromRow =
    PersistedUser
      <$> (   Entity
          <$> (ID <$> field)
          <*> (User <$> field <*> field <*> field <*> field <*> field)
          )
