{-# LANGUAGE OverloadedStrings #-}
module Serializers where

import Data.Text
import Data.Time as Time

data SerializedUser = SerializedUser {
    serializedUserId :: Int,
    serializedUserFirstName :: Text,
    serializedUserLastName :: Text,
    serializedUserPicture :: SerializedPicture,
    serializedUserCreatedOn :: Time.UTCTime,
    serializedUserIsAdmin :: Bool,
    serializedUserComments :: [SerializedComment]
} deriving Show

data SerializedAuthor = SerializedAuthor {
    serializedAuthorId :: Int,
    serializedAuthorUser :: SerializedUser,
    serializedAuthorDescription :: Text,
    serializedAuthorNews :: [SerializedNews]
} deriving Show

data SerializedNews = SerializedNews {
    serializedNewsId :: Int,
    serializedNewsTitle :: Text,
    serializedNewsCreatedOn :: LocalTime,
    serializedNewsAuthor :: SerializedAuthor,
    serializedNewsCategory :: SerializedCategory,
    serializedNewsTags :: [SerializedTag],
    serializedNewsComments :: [SerializedComment],
    serializedNewsPicture :: SerializedPicture,
    serializedNewsPictures :: [SerializedPicture],
    serializedNewsContent :: Text,
    serializedNewsIsDraft :: Bool
} deriving Show

data SerializedCategory = SerializedCategory {
    serializedCategoryId :: Int,
    serializedCategoryName :: Text
} deriving Show

data SerializedTag = SerializedTag {
    serializedTagId :: Int,
    serializedTagName :: Text
} deriving Show

data SerializedPicture = SerializedPicture {
    serializedPictureId :: Int,
    serializedPictureFilePath :: Text
} deriving Show

data SerializedComment = SerializedComment {
    serializedCommentId :: Int,
    serializedCommentUser :: SerializedUser,
    serializedCommentText :: Text
} deriving Show

serializeUser userData = undefined

serializeAuthor authorData = undefined

serializerNews newsData = undefined

serializeTag tagData = undefined

serializeCategory categoryData = undefined

serializedComment commentData = undefined

serializedPicture pictureData = undefined