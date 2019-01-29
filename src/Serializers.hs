{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Serializers where

import           Data.Text
import           Data.Aeson
import           Models
import           GHC.Generics
import           Data.Time                     as Time

data UserResponse =
    UserSuccessResponse {
        successUserId :: Int,
        successUserFirstName :: Text,
        successUserLastName :: Text,
        -- successUserPicture :: Text,
        successUserCreatedOn :: Time.UTCTime,
        successUserIsAdmin :: Bool
        -- serializedUserComments :: [SerializedComment]
    } deriving Show

instance ToJSON UserResponse where
    toJSON UserSuccessResponse {..} = object
        [ "id" .= successUserId
        , "first_name" .= successUserFirstName
        , "last_name" .= successUserLastName
        , "created_on" .= successUserCreatedOn
        , "is_admin" .= successUserIsAdmin
        ]

-- serializeError :: Text -> UserResponse
-- serializeError err = ErrorResponse err 0

serializeUserSuccess :: User -> UserResponse
serializeUserSuccess User {..} = UserSuccessResponse
    { successUserId        = userId
    , successUserFirstName = userFirstName
    , successUserLastName  = userLastName
                                    --   , successUserPicture   = userPicture
    , successUserCreatedOn = userCreatedOn
    , successUserIsAdmin   = userIsAdmin
    }

data CreateUserRaw = CreateUserRaw {
    createUserRawFirstName :: Text,
    createUserRawLastName :: Text,
    createUserRawIsAdmin :: Maybe Bool
} deriving (Show, Eq, Generic)

instance FromJSON CreateUserRaw where
    parseJSON (Object v) =
        CreateUserRaw
            <$> v
            .:  "first_name"
            <*> v
            .:  "last_name"
            <*> v
            .:! "is_admin"

data SerializedAuthor = SerializedAuthor {
    serializedAuthorId :: Int,
    serializedAuthorUser :: UserResponse,
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
    serializedCommentUser :: Int,
    serializedCommentText :: Text
} deriving Show
