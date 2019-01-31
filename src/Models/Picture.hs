{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Models.Picture where

import qualified Data.Text                     as T
import qualified Core.Time                     as Time
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField

data Picture = Picture {
    pictureId :: Int,
    pictureCreatedOn :: Time.UTCTime,
    pictureFilePath :: T.Text
} deriving Show

pictureDefault = Picture { pictureId        = -1
                         , pictureCreatedOn = Time.defaultToday
                         , pictureFilePath  = ""
                         }

instance FromRow Picture where
    fromRow = Picture <$> field <*> field <*> field

instance ToRow Picture where
    toRow Picture {..} =
        [toField pictureId, toField pictureCreatedOn, toField pictureFilePath]
