{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Models.Category where

import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import qualified Data.Text as T

data Category = Category {
    categoryId :: Int,
    categoryName :: T.Text
} deriving Show

defaultCategory = Category { categoryId = -1, categoryName = "" }

instance FromRow Category where
    fromRow = Category <$> field <*> field

instance ToRow Category where
    toRow Category {..} = [toField categoryId, toField categoryName]