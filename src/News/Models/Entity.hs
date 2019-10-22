module News.Models.Entity where

newtype ID v =
  ID Int
  deriving (Show, Eq)

fromID :: ID v -> Int
fromID (ID eId) = eId

toID :: Int -> ID v
toID = ID

data Entity v = Entity
  { getID :: ID v
  , getValue :: v
  } deriving (Show, Eq)
