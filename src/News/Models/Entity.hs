module News.Models.Entity where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow

newtype ID v =
  ID Int
  deriving (Show, Eq)

instance ToRow (ID v) where
  toRow (ID obj) = [toField obj]

fromID :: ID v -> Int
fromID (ID eId) = eId

toID :: Int -> ID v
toID = ID

data Entity v = Entity
  { getId :: ID v
  , getObj :: v
  } deriving (Show, Eq)
