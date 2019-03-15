{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Models.Post where

import qualified Core.Time as Time
import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

data Post = Post
  { postId :: Int
  , postTitle :: T.Text
  , postCreatedOn :: Time.UTCTime
  , postAuthor :: Int
  , postCategory :: Maybe Int
  , postContent :: T.Text
  , postPicture :: Maybe Int
  , postIsDraft :: Bool
  } deriving (Show)

defaultPost =
  Post
    { postId = -1
    , postTitle = ""
    , postCreatedOn = Time.defaultToday
    , postAuthor = -1
    , postCategory = Nothing
    , postContent = ""
    , postPicture = Nothing
    , postIsDraft = False
    }

instance FromRow Post where
  fromRow =
    Post <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*>
    field

instance ToRow Post where
  toRow Post {..} =
    [ toField postId
    , toField postTitle
    , toField postCreatedOn
    , toField postAuthor
    , toField postCategory
    , toField postContent
    , toField postPicture
    , toField postIsDraft
    ]
