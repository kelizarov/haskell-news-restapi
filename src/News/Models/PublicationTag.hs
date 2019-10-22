module News.Models.PublicationTag where

import qualified Data.Text                     as T

data PublicationTag = PublicationTag
  { ptValue :: T.Text
  } deriving (Show, Eq)
