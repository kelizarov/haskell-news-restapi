module News.Models.Author where

import qualified Data.Text                     as T

import           News.Models.User               ( User )

data Author = Author
  { aUser :: User
  , aShortDescription :: T.Text
  } deriving (Show, Eq)
