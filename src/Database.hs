{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Database where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Migration
import           Control.Exception
import           Models


connectInfo :: ConnectInfo
connectInfo = ConnectInfo { connectHost     = ""
                          , connectPort     = 5432
                          , connectUser     = "admin"
                          , connectPassword = ""
                          , connectDatabase = "news_restapi"
                          }

migrate :: IO ()
migrate = bracket (connect connectInfo) close $ \conn -> do
    result <- withTransaction conn (runMigrations False conn cmds)
    case result of
        MigrationError err -> error err
        _                  -> return ()
    where cmds = [MigrationInitialization, MigrationDirectory "./migrations"]

createUser :: User -> IO User
createUser User {..} = bracket (connect connectInfo) close $ \conn -> do
    (user : _) <- query conn userQuery (userFirstName, userLastName)
    pure user
  where
    userQuery
        = "INSERT INTO users(first_name, last_name, created_on) \
            \VALUES (?, ?, CURRENT_TIMESTAMP) RETURNING *;"

createAuthor :: Author -> IO Author
createAuthor Author {..} = bracket (connect connectInfo) close $ \conn -> do
    (author : _) <- query conn authorQuery (authorUserId, authorDescription)
    pure author
  where
    authorQuery =
        "INSERT INTO authors(user_id, description) VALUES (?, ?) RETURNING *;"

createNews :: News -> IO News
createNews News {..} = bracket (connect connectInfo) close $ \conn -> do
    (news : _) <- query
        conn
        newsQuery
        ( newsTitle
        , newsAuthor
        , newsCategory
        , newsContent
        , newsPicture
        , newsIsDraft
        )
    pure news
  where
    newsQuery
        = "INSERT INTO news(title, author, category, content, picture, is_draft) \
        \ VALUES (?, ?, ?, ?, ?, ?)\
        \ RETURNING *;"

createCategory :: Category -> IO Category
createCategory Category {..} = bracket (connect connectInfo) close $ \conn ->
    do
        (category : _) <- query conn categoryQuery [categoryName]
        pure category
  where
    categoryQuery = "INSERT INTO news_categories(name) VALUES (?) RETURNING *;"

createTag :: Tag -> IO Tag
createTag Tag {..} = bracket (connect connectInfo) close $ \conn -> do
    (tag : _) <- query conn tagQuery [tagName]
    pure tag
    where tagQuery = "INSERT INTO tags(name) VALUES (?) RETURNING *;"

createPicture :: Picture -> IO Picture
createPicture Picture {..} = bracket (connect connectInfo) close $ \conn -> do
    (picture : _) <- query conn pictureQuery [pictureFilePath]
    pure picture
  where
    pictureQuery = "INSERT INTO pictures(file_path) VALUES (?) RETURNING *;"

createComment :: Comment -> IO Comment
createComment Comment {..} = bracket (connect connectInfo) close $ \conn -> do
    (comment : _) <- query conn commentQuery (commentNewsId, commentUserId, commentText)
    pure comment
  where
    commentQuery
        = "INSERT INTO news_comments(news_id, user_id, text) \
        \VALUES (?, ?, ?) RETURNING *;"

getUser :: Int -> IO User
getUser userId = undefined

getAuthor :: Int -> IO Author
getAuthor authorId = undefined

getNews :: Int -> IO News
getNews newsId = undefined

getCategory :: Int -> IO Category
getCategory categoryId = undefined

getTag :: Int -> IO Tag
getTag tagId = undefined

getPicture :: Int -> IO Picture
getPicture pictureId = undefined

getComment :: Int -> IO Comment
getComment commentId = undefined
