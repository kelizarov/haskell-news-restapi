{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Serializers where

import           Data.Text
import           Data.Aeson
import           Models
import           GHC.Generics
import           Data.Time                     as Time

data SerializedUser = SerializedUser {
    serializedUserId :: Int,
    serializedUserFirstName :: Text,
    serializedUserLastName :: Text,
    -- serializedUserPicture :: SerializedPicture,
    serializedUserCreatedOn :: Time.UTCTime,
    serializedUserIsAdmin :: Bool
    -- serializedUserComments :: [SerializedComment]
} deriving Show

instance ToJSON SerializedUser where
    toJSON SerializedUser {..} = object
        [ "id" .= serializedUserId
        , "first_name" .= serializedUserFirstName
        , "last_name" .= serializedUserLastName
        , "created_on" .= serializedUserCreatedOn
        , "is_admin" .= serializedUserIsAdmin
        ]

serializeUser :: User -> SerializedUser
serializeUser User {..} = SerializedUser
    { serializedUserId        = userId
    , serializedUserFirstName = userFirstName
    , serializedUserLastName  = userLastName
    , serializedUserCreatedOn = userCreatedOn
    , serializedUserIsAdmin   = userIsAdmin
    }

data CreateUserRaw = CreateUserRaw {
    createUserRawFirstName :: Text,
    createUserRawLastName :: Text
} deriving (Show, Eq, Generic)

instance FromJSON CreateUserRaw where
    parseJSON (Object v) =
        CreateUserRaw <$> v .: "first_name" <*> v .: "last_name"

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
