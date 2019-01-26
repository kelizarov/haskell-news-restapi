{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Models where

import           Data.Text
import           Data.Time                     as Time
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField

defaultToday :: Time.Day
defaultToday = Time.fromGregorian 1970 1 1

data User = User {
    userId :: Int,
    userFirstName :: Text,
    userLastName :: Text,
    userPicture :: Maybe Int,
    userCreatedOn :: Time.UTCTime,
    userIsAdmin :: Bool
} deriving Show

userDefault = User { userId        = -1
                   , userFirstName = ""
                   , userLastName  = ""
                   , userPicture   = Nothing
                   , userCreatedOn = UTCTime defaultToday 0
                   , userIsAdmin   = False
                   }

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
    toRow User {..} =
        [ toField userId
        , toField userFirstName
        , toField userLastName
        , toField userPicture
        , toField userCreatedOn
        , toField userIsAdmin
        ]

data Author = Author {
    authorId :: Int,
    authorUserId :: Int,
    authorDescription :: Text
} deriving Show

authorDefault =
    Author { authorId = -1, authorUserId = -1, authorDescription = "" }

instance FromRow Author where
    fromRow = Author <$> field <*> field <*> field

instance ToRow Author where
    toRow Author {..} =
        [toField authorId, toField authorUserId, toField authorDescription]

data Category = Category {
    categoryId :: Int,
    categoryName :: Text
} deriving Show

categoryDefault = Category { categoryId = -1, categoryName = "" }

instance FromRow Category where
    fromRow = Category <$> field <*> field

instance ToRow Category where
    toRow Category {..} = [toField categoryId, toField categoryName]

data Tag = Tag {
    tagId :: Int,
    tagName :: Text
} deriving Show

tagDefault = Tag { tagId = -1, tagName = "" }

instance FromRow Tag where
    fromRow = Tag <$> field <*> field

instance ToRow Tag where
    toRow Tag {..} = [toField tagId, toField tagName]

data Comment = Comment {
    commentId :: Int,
    commentCreatedOn :: Time.UTCTime,
    commentNewsId :: Int,
    commentUserId :: Int,
    commentText :: Text
} deriving Show

commentDefault = Comment { commentId        = -1
                         , commentCreatedOn = UTCTime defaultToday 0
                         , commentNewsId    = -1
                         , commentUserId    = -1
                         , commentText      = ""
                         }

instance FromRow Comment where
    fromRow = Comment <$> field <*> field <*> field <*> field <*> field

instance ToRow Comment where
    toRow Comment {..} =
        [ toField commentId
        , toField commentCreatedOn
        , toField commentNewsId
        , toField commentUserId
        , toField commentText
        ]

data Picture = Picture {
    pictureId :: Int,
    pictureCreatedOn :: Time.UTCTime,
    pictureFilePath :: Text
} deriving Show

pictureDefault = Picture { pictureId        = -1
                         , pictureCreatedOn = UTCTime defaultToday 0
                         , pictureFilePath  = ""
                         }

instance FromRow Picture where
    fromRow = Picture <$> field <*> field <*> field

instance ToRow Picture where
    toRow Picture {..} =
        [toField pictureId, toField pictureCreatedOn, toField pictureFilePath]

data News = News {
    newsId :: Int,
    newsTitle :: Text,
    newsCreatedOn :: Time.UTCTime,
    newsAuthor :: Int,
    newsCategory :: Maybe Int,
    newsContent :: Text,
    newsPicture :: Maybe Int,
    newsIsDraft :: Bool
} deriving Show

defaultNews = News { newsId        = -1
                   , newsTitle     = ""
                   , newsCreatedOn = UTCTime defaultToday 0
                   , newsAuthor    = -1
                   , newsCategory  = Nothing
                   , newsContent   = ""
                   , newsPicture   = Nothing
                   , newsIsDraft   = False
                   }

instance FromRow News where
    fromRow =
        News
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

instance ToRow News where
    toRow News {..} =
        [ toField newsId
        , toField newsTitle
        , toField newsCreatedOn
        , toField newsAuthor
        , toField newsCategory
        , toField newsContent
        , toField newsPicture
        , toField newsIsDraft
        ]
