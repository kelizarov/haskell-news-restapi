module News.Models.User where

import qualified Data.Text                     as T
import qualified Data.Time                     as Time

data User = User
  { uFirstName :: T.Text
  , uLastName :: T.Text
  , uAvatarPath :: T.Text
  , uCreatedAt :: Time.UTCTime
  , uIsAdmin :: Bool
  } deriving (Show, Eq)
