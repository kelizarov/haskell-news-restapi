module News.Models.User where

import qualified Data.Text                     as T
import qualified Data.Time                     as Time
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow

import           News.Models.Persisted

data User = User
  { uFirstName :: T.Text
  , uLastName :: T.Text
  , uAvatarPath :: T.Text
  , uCreatedAt :: Time.UTCTime
  , uIsAdmin :: Bool
  } deriving (Show, Eq)
