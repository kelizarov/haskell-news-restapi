{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Models.Comment where

import qualified Core.Time as Time
import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

data Comment = Comment
  { commentId :: Int
  , commentCreatedOn :: Time.UTCTime
  , commentPostId :: Int
  , commentUserId :: Int
  , commentText :: T.Text
  } deriving (Show)

commentDefault =
  Comment
    { commentId = -1
    , commentCreatedOn = Time.defaultToday
    , commentPostId = -1
    , commentUserId = -1
    , commentText = ""
    }

instance FromRow Comment where
  fromRow = Comment <$> field <*> field <*> field <*> field <*> field

instance ToRow Comment where
  toRow Comment {..} =
    [ toField commentId
    , toField commentCreatedOn
    , toField commentPostId
    , toField commentUserId
    , toField commentText
    ]
