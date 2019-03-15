{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Models.Picture where

import qualified Core.Time as Time
import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

data Picture = Picture
  { pictureId :: Int
  , pictureCreatedOn :: Time.UTCTime
  , pictureFilePath :: T.Text
  } deriving (Show)

pictureDefault =
  Picture
    {pictureId = -1, pictureCreatedOn = Time.defaultToday, pictureFilePath = ""}

instance FromRow Picture where
  fromRow = Picture <$> field <*> field <*> field

instance ToRow Picture where
  toRow Picture {..} =
    [toField pictureId, toField pictureCreatedOn, toField pictureFilePath]
