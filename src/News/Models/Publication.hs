module News.Models.Publication where

import qualified Data.Text as T

data Publication
  = Publication
      { pTitle :: T.Text,
        pBody :: T.Text
      }
  deriving (Show, Eq)
