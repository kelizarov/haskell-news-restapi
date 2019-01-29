{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Database where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Migration
import           Data.Text
import           Control.Exception
import           Models


connectInfo :: ConnectInfo
connectInfo = ConnectInfo { connectHost     = ""
                          , connectPort     = 5432
                          , connectUser     = "admina"
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

insertUser :: Exception e => User -> IO (Either e User)
insertUser User {..} = try $ bracket (connect connectInfo) close $ \conn -> do
    (obj : _) <- query conn q (userFirstName, userLastName, userIsAdmin)
    pure obj
  where
    q
        = "INSERT INTO users(first_name, last_name, is_admin, created_on) \
                \VALUES (?, ?, ?, CURRENT_TIMESTAMP) RETURNING *;"

insertAuthor :: Exception e => Author -> IO (Either e Author)
insertAuthor Author {..} =
    try $ bracket (connect connectInfo) close $ \conn -> do
        (obj : _) <- query conn q (authorUserId, authorDescription)
        pure obj
  where
    q = "INSERT INTO authors(user_id, description) VALUES (?, ?) RETURNING *;"

insertNews :: Exception e => News -> IO (Either e News)
insertNews News {..} = try $ bracket (connect connectInfo) close $ \conn -> do
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

insertCategory :: Exception e => Category -> IO (Either e Category)
insertCategory Category {..} =
    try $ bracket (connect connectInfo) close $ \conn -> do
        (category : _) <- query conn q [categoryName]
        pure category
    where q = "INSERT INTO news_categories(name) VALUES (?) RETURNING *;"

insertTag :: Exception e => Tag -> IO (Either e Tag)
insertTag Tag {..} = try $ bracket (connect connectInfo) close $ \conn -> do
    (obj : _) <- query conn q [tagName]
    pure obj
    where q = "INSERT INTO tags(name) VALUES (?) RETURNING *;"

insertPicture :: Exception e => Picture -> IO (Either e Picture)
insertPicture Picture {..} =
    try $ bracket (connect connectInfo) close $ \conn -> do
        (obj : _) <- query conn q [pictureFilePath]
        pure obj
    where q = "INSERT INTO pictures(file_path) VALUES (?) RETURNING *;"

insertComment :: Exception e => Comment -> IO (Either e Comment)
insertComment Comment {..} =
    try $ bracket (connect connectInfo) close $ \conn -> do
        (obj : _) <- query conn q (commentNewsId, commentUserId, commentText)
        pure obj
  where
    q
        = "INSERT INTO news_comments(news_id, user_id, text, created_on) \
        \VALUES (?, ?, ?, CURRENT_TIMESTAMP) RETURNING *;"

selectUser :: Exception e => Int -> IO (Either e (Maybe User))
selectUser id = try $ bracket (connect connectInfo) close $ \conn -> do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM users WHERE id = ?;"

selectAuthor :: Exception e => Int -> IO (Either e (Maybe Author))
selectAuthor id = try $ bracket (connect connectInfo) close $ \conn -> do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM authors WHERE id = ?;"

selectNews :: Exception e => Int -> IO (Either e (Maybe News))
selectNews id = try $ bracket (connect connectInfo) close $ \conn -> do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM news_objects WHERE id = ?;"

selectCategory :: Exception e => Int -> IO (Either e (Maybe Category))
selectCategory id = try $ bracket (connect connectInfo) close $ \conn -> do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM news_categories WHERE id = ?;"

selectTag :: Exception e => Int -> IO (Either e (Maybe Tag))
selectTag id = try $ bracket (connect connectInfo) close $ \conn -> do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM tags WHERE id = ?;"

selectPicture :: Exception e => Int -> IO (Either e (Maybe Picture))
selectPicture id = try $ bracket (connect connectInfo) close $ \conn -> do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM pictures WHERE id = ?;"

selectComment :: Exception e => Int -> IO (Either e (Maybe Comment))
selectComment id = try $ bracket (connect connectInfo) close $ \conn -> do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM news_comments WHERE id = ?;"

updateUser :: Exception e => User -> IO (Either e (Maybe User))
updateUser User {..} = try $ bracket (connect connectInfo) close $ \conn -> do
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

updateAuthor :: Exception e => Author -> IO (Either e (Maybe Author))
updateAuthor Author {..} = try $ bracket (connect connectInfo) close $ \conn -> do
    res <- query conn q (authorDescription, authorId)
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "UPDATE authors SET (description) = (?) WHERE id = ?;"

updateNews :: Exception e => News -> IO (Either e (Maybe News))
updateNews News {..} = try $ bracket (connect connectInfo) close $ \conn -> do
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

updateComment :: Exception e => Comment -> IO (Either e (Maybe Comment))
updateComment Comment {..} = try $ bracket (connect connectInfo) close $ \conn -> do
    res <- query conn q (commentText, commentText)
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "UPDATE news_comments SET (text) = (?) WHERE id = ?;"
