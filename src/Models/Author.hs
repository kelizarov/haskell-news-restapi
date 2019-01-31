{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Author where

import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import qualified Data.Text                     as T

data Author = Author {
    authorId :: Int,
    authorUserId :: Int,
    authorDescription :: T.Text
} deriving Show

defaultAuthor =
    Author { authorId = -1, authorUserId = -1, authorDescription = "" }

instance FromRow Author where
    fromRow = Author <$> field <*> field <*> field

instance ToRow Author where
    toRow Author {..} =
        [toField authorId, toField authorUserId, toField authorDescription]
