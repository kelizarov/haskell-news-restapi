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

insertUser :: User -> IO User
insertUser User {..} = bracket (connect connectInfo) close $ \conn -> do
    (obj : _) <- query conn q (userFirstName, userLastName)
    pure obj
  where
    q
        = "INSERT INTO users(first_name, last_name, created_on) \
            \VALUES (?, ?, CURRENT_TIMESTAMP) RETURNING *;"

insertAuthor :: Author -> IO Author
insertAuthor Author {..} = bracket (connect connectInfo) close $ \conn -> do
    (obj : _) <- query conn q (authorUserId, authorDescription)
    pure obj
  where
    q = "INSERT INTO authors(user_id, description) VALUES (?, ?) RETURNING *;"

insertNews :: News -> IO News
insertNews News {..} = bracket (connect connectInfo) close $ \conn -> do
    (obj : _) <- query
        conn
        q
        ( newsTitle
        , newsAuthor
        , newsCategory
        , newsContent
        , newsPicture
        , newsIsDraft
        )
    pure obj
  where
    q
        = "INSERT INTO news_objects(title, author, category, content, picture, is_draft, created_on) \
        \ VALUES (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)\
        \ RETURNING *;"

insertCategory :: Category -> IO Category
insertCategory Category {..} = bracket (connect connectInfo) close $ \conn ->
    do
        (category : _) <- query conn q [categoryName]
        pure category
    where q = "INSERT INTO news_categories(name) VALUES (?) RETURNING *;"

insertTag :: Tag -> IO Tag
insertTag Tag {..} = bracket (connect connectInfo) close $ \conn -> do
    (obj : _) <- query conn q [tagName]
    pure obj
    where q = "INSERT INTO tags(name) VALUES (?) RETURNING *;"

insertPicture :: Picture -> IO Picture
insertPicture Picture {..} = bracket (connect connectInfo) close $ \conn -> do
    (obj : _) <- query conn q [pictureFilePath]
    pure obj
    where q = "INSERT INTO pictures(file_path) VALUES (?) RETURNING *;"

insertComment :: Comment -> IO Comment
insertComment Comment {..} = bracket (connect connectInfo) close $ \conn -> do
    (obj : _) <- query conn q (commentNewsId, commentUserId, commentText)
    pure obj
  where
    q
        = "INSERT INTO news_comments(news_id, user_id, text, created_on) \
        \VALUES (?, ?, ?, CURRENT_TIMESTAMP) RETURNING *;"

selectUser :: Int -> IO (Maybe User)
selectUser id = bracket (connect connectInfo) close $ \conn -> do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM users WHERE id = ?;"

selectAuthor :: Int -> IO (Maybe Author)
selectAuthor id = bracket (connect connectInfo) close $ \conn -> do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM authors WHERE id = ?;"

selectNews :: Int -> IO (Maybe News)
selectNews id = bracket (connect connectInfo) close $ \conn -> do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM news_objects WHERE id = ?;"

selectCategory :: Int -> IO (Maybe Category)
selectCategory id = bracket (connect connectInfo) close $ \conn -> do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM news_categories WHERE id = ?;"

selectTag :: Int -> IO (Maybe Tag)
selectTag id = bracket (connect connectInfo) close $ \conn -> do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM tags WHERE id = ?;"

selectPicture :: Int -> IO (Maybe Picture)
selectPicture id = bracket (connect connectInfo) close $ \conn -> do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM pictures WHERE id = ?;"

selectComment :: Int -> IO (Maybe Comment)
selectComment id = bracket (connect connectInfo) close $ \conn -> do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM news_comments WHERE id = ?;"

updateUser :: User -> IO (Maybe User)
updateUser User {..} = bracket (connect connectInfo) close $ \conn -> do
    res <- query
        conn
        q
        (userFirstName, userLastName, userPicture, userIsAdmin, userId)
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
  where
    q
        = "UPDATE users SET (first_name, last_name, user_picture, is_admin) = \
        \(?, ?, ?, ?) WHERE id = ? RETURNING *; "

updateAuthor :: Author -> IO (Maybe Author)
updateAuthor Author {..} = bracket (connect connectInfo) close $ \conn -> do
    res <- query conn q (authorDescription, authorId)
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "UPDATE authors SET (description) = (?) WHERE id = ?;"

updateNews :: News -> IO (Maybe News)
updateNews News {..} = bracket (connect connectInfo) close $ \conn -> do
    res <- query
        conn
        q
        (newsTitle, newsCategory, newsContent, newsPicture, newsIsDraft, newsId)
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
  where
    q
        = "UPDATE news_objects SET (title, category, content, picture, is_draft)\
        \ = (?, ?, ?, ?, ?) WHERE id = ?;"

updateComment :: Comment -> IO (Maybe Comment)
updateComment Comment {..} = bracket (connect connectInfo) close $ \conn -> do
    res <- query conn q (commentText, commentText)
    case res of
        [] -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "UPDATE news_comments SET (text) = (?) WHERE id = ?;"