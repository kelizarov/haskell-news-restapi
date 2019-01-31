{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Models.Tag where

import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import qualified Data.Text as T

data Tag = Tag {
    tagId :: Int,
    tagName :: T.Text
} deriving Show

defaultTag = Tag { tagId = -1, tagName = "" }

instance FromRow Tag where
    fromRow = Tag <$> field <*> field

instance ToRow Tag where
    toRow Tag {..} = [toField tagId, toField tagName]
