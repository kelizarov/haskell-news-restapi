{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Database where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Migration
import           Data.Text
import           Control.Exception
import           Control.Monad.IO.Class
import           Models
import qualified Config                        as C

connectInfo :: C.Config -> IO ConnectInfo
connectInfo conf = do
    host     <- C.get conf "database.host"
    port     <- C.get conf "database.port"
    user     <- C.get conf "database.user"
    password <- C.get conf "database.password"
    database <- C.get conf "database.database"
    pure $ ConnectInfo { connectHost     = host
                       , connectPort     = port
                       , connectUser     = user
                       , connectPassword = password
                       , connectDatabase = database
                       }

migrate :: Connection -> IO ()
migrate conn = do
    result <- withTransaction conn (runMigrations False conn cmds)
    case result of
        MigrationError err -> error err
        _                  -> return ()
    where cmds = [MigrationInitialization, MigrationDirectory "./migrations"]

insertUser :: Exception e => Connection -> User -> IO (Either e User)
insertUser conn User {..} = try $ do
    (obj : _) <- query conn q (userFirstName, userLastName, userIsAdmin)
    pure obj
  where
    q
        = "INSERT INTO users(first_name, last_name, is_admin, created_on) \
                    \VALUES (?, ?, ?, CURRENT_TIMESTAMP) RETURNING *;"

insertAuthor :: Exception e => Connection -> Author -> IO (Either e Author)
insertAuthor conn Author {..} = try $ do
    (obj : _) <- query conn q (authorUserId, authorDescription)
    pure obj
  where
    q = "INSERT INTO authors(user_id, description) VALUES (?, ?) RETURNING *;"

insertNews :: Exception e => Connection -> News -> IO (Either e News)
insertNews conn News {..} = try $ do
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

insertCategory
    :: Exception e => Connection -> Category -> IO (Either e Category)
insertCategory conn Category {..} = try $ do
    (category : _) <- query conn q [categoryName]
    pure category
    where q = "INSERT INTO news_categories(name) VALUES (?) RETURNING *;"

insertTag :: Exception e => Connection -> Tag -> IO (Either e Tag)
insertTag conn Tag {..} = try $ do
    (obj : _) <- query conn q [tagName]
    pure obj
    where q = "INSERT INTO tags(name) VALUES (?) RETURNING *;"

insertPicture :: Exception e => Connection -> Picture -> IO (Either e Picture)
insertPicture conn Picture {..} = try $ do
    (obj : _) <- query conn q [pictureFilePath]
    pure obj
    where q = "INSERT INTO pictures(file_path) VALUES (?) RETURNING *;"

insertComment :: Exception e => Connection -> Comment -> IO (Either e Comment)
insertComment conn Comment {..} = try $ do
    (obj : _) <- query conn q (commentNewsId, commentUserId, commentText)
    pure obj
  where
    q
        = "INSERT INTO news_comments(news_id, user_id, text, created_on) \
        \VALUES (?, ?, ?, CURRENT_TIMESTAMP) RETURNING *;"

selectUser :: Exception e => Connection -> Int -> IO (Either e (Maybe User))
selectUser conn id = try $ do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM users WHERE id = ?;"

selectAuthor :: Exception e => Connection -> Int -> IO (Either e (Maybe Author))
selectAuthor conn id = try $ do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM authors WHERE id = ?;"

selectNews :: Exception e => Connection -> Int -> IO (Either e (Maybe News))
selectNews conn id = try $ do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM news_objects WHERE id = ?;"

selectCategory
    :: Exception e => Connection -> Int -> IO (Either e (Maybe Category))
selectCategory conn id = try $ do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM news_categories WHERE id = ?;"

selectTag :: Exception e => Connection -> Int -> IO (Either e (Maybe Tag))
selectTag conn id = try $ do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM tags WHERE id = ?;"

selectPicture
    :: Exception e => Connection -> Int -> IO (Either e (Maybe Picture))
selectPicture conn id = try $ do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM pictures WHERE id = ?;"

selectComment
    :: Exception e => Connection -> Int -> IO (Either e (Maybe Comment))
selectComment conn id = try $ do
    res <- query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM news_comments WHERE id = ?;"

updateUser :: Exception e => Connection -> User -> IO (Either e (Maybe User))
updateUser conn User {..} = try $ do
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

updateAuthor
    :: Exception e => Connection -> Author -> IO (Either e (Maybe Author))
updateAuthor conn Author {..} = try $ do
    res <- query conn q (authorDescription, authorId)
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "UPDATE authors SET (description) = (?) WHERE id = ?;"

updateNews :: Exception e => Connection -> News -> IO (Either e (Maybe News))
updateNews conn News {..} = try $ do
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

updateComment
    :: Exception e => Connection -> Comment -> IO (Either e (Maybe Comment))
updateComment conn Comment {..} = try $ do
    res <- query conn q (commentText, commentText)
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "UPDATE news_comments SET (text) = (?) WHERE id = ?;"
