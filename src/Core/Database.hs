{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Core.Database
    ( PSQL.Connection
    , connect
    , selectUser
    , insertUser
    , updateUser
    )
where

import           Database.PostgreSQL.Simple.Migration
import           Data.Text
import           Control.Exception
import           Control.Monad.IO.Class
import           Models.User
import           Models.Author
import           Models.Post
import           Models.Picture
import           Models.Tag
import           Models.Category
import           Models.Comment
import qualified Core.Config                   as C
import qualified Database.PostgreSQL.Simple    as PSQL

connectInfo :: C.Config -> IO PSQL.ConnectInfo
connectInfo conf = do
    host     <- C.get conf "database.host"
    port     <- C.get conf "database.port"
    user     <- C.get conf "database.user"
    password <- C.get conf "database.password"
    database <- C.get conf "database.database"
    pure $ PSQL.ConnectInfo { connectHost     = host
                            , connectPort     = port
                            , connectUser     = user
                            , connectPassword = password
                            , connectDatabase = database
                            }

connect :: C.Config -> IO PSQL.Connection
connect conf = connectInfo conf >>= PSQL.connect

migrate :: PSQL.Connection -> IO ()
migrate conn = do
    result <- PSQL.withTransaction conn (runMigrations False conn cmds)
    case result of
        MigrationError err -> error err
        _                  -> return ()
    where cmds = [MigrationInitialization, MigrationDirectory "./migrations"]

insertUser :: Exception e => PSQL.Connection -> User -> IO (Either e User)
insertUser conn User {..} = try $ do
    (obj : _) <- PSQL.query conn q (userFirstName, userLastName, userIsAdmin)
    pure obj
  where
    q
        = "INSERT INTO users(first_name, last_name, is_admin, created_on) \
                    \VALUES (?, ?, ?, CURRENT_TIMESTAMP) RETURNING *;"

selectUser
    :: Exception e => PSQL.Connection -> Int -> IO (Either e (Maybe User))
selectUser conn id = try $ do
    res <- PSQL.query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM users WHERE id = ?;"

updateUser
    :: Exception e => PSQL.Connection -> User -> IO (Either e (Maybe User))
updateUser conn User {..} = try $ do
    res <- PSQL.query
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

insertAuthor :: Exception e => PSQL.Connection -> Author -> IO (Either e Author)
insertAuthor conn Author {..} = try $ do
    (obj : _) <- PSQL.query conn q (authorUserId, authorDescription)
    pure obj
  where
    q = "INSERT INTO authors(user_id, description) VALUES (?, ?) RETURNING *;"

insertPost :: Exception e => PSQL.Connection -> Post -> IO (Either e Post)
insertPost conn Post {..} = try $ do
    (obj : _) <- PSQL.query
        conn
        q
        ( postTitle
        , postAuthor
        , postCategory
        , postContent
        , postPicture
        , postIsDraft
        )
    pure obj
  where
    q
        = "INSERT INTO post_objects(title, author, category, content, picture, is_draft, created_on) \
        \ VALUES (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)\
        \ RETURNING *;"

insertCategory
    :: Exception e => PSQL.Connection -> Category -> IO (Either e Category)
insertCategory conn Category {..} = try $ do
    (category : _) <- PSQL.query conn q [categoryName]
    pure category
    where q = "INSERT INTO post_categories(name) VALUES (?) RETURNING *;"

insertTag :: Exception e => PSQL.Connection -> Tag -> IO (Either e Tag)
insertTag conn Tag {..} = try $ do
    (obj : _) <- PSQL.query conn q [tagName]
    pure obj
    where q = "INSERT INTO tags(name) VALUES (?) RETURNING *;"

insertPicture
    :: Exception e => PSQL.Connection -> Picture -> IO (Either e Picture)
insertPicture conn Picture {..} = try $ do
    (obj : _) <- PSQL.query conn q [pictureFilePath]
    pure obj
    where q = "INSERT INTO pictures(file_path) VALUES (?) RETURNING *;"

insertComment
    :: Exception e => PSQL.Connection -> Comment -> IO (Either e Comment)
insertComment conn Comment {..} = try $ do
    (obj : _) <- PSQL.query conn q (commentPostId, commentUserId, commentText)
    pure obj
  where
    q
        = "INSERT INTO post_comments(post_id, user_id, text, created_on) \
        \VALUES (?, ?, ?, CURRENT_TIMESTAMP) RETURNING *;"


selectAuthor
    :: Exception e => PSQL.Connection -> Int -> IO (Either e (Maybe Author))
selectAuthor conn id = try $ do
    res <- PSQL.query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM authors WHERE id = ?;"

selectPost
    :: Exception e => PSQL.Connection -> Int -> IO (Either e (Maybe Post))
selectPost conn id = try $ do
    res <- PSQL.query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM post_objects WHERE id = ?;"

selectCategory
    :: Exception e => PSQL.Connection -> Int -> IO (Either e (Maybe Category))
selectCategory conn id = try $ do
    res <- PSQL.query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM post_categories WHERE id = ?;"

selectTag :: Exception e => PSQL.Connection -> Int -> IO (Either e (Maybe Tag))
selectTag conn id = try $ do
    res <- PSQL.query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM tags WHERE id = ?;"

selectPicture
    :: Exception e => PSQL.Connection -> Int -> IO (Either e (Maybe Picture))
selectPicture conn id = try $ do
    res <- PSQL.query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM pictures WHERE id = ?;"

selectComment
    :: Exception e => PSQL.Connection -> Int -> IO (Either e (Maybe Comment))
selectComment conn id = try $ do
    res <- PSQL.query conn q [id]
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "SELECT * FROM post_comments WHERE id = ?;"

updateAuthor
    :: Exception e => PSQL.Connection -> Author -> IO (Either e (Maybe Author))
updateAuthor conn Author {..} = try $ do
    res <- PSQL.query conn q (authorDescription, authorId)
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "UPDATE authors SET (description) = (?) WHERE id = ?;"

updatePost
    :: Exception e => PSQL.Connection -> Post -> IO (Either e (Maybe Post))
updatePost conn Post {..} = try $ do
    res <- PSQL.query
        conn
        q
        (postTitle, postCategory, postContent, postPicture, postIsDraft, postId)
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
  where
    q
        = "UPDATE post_objects SET (title, category, content, picture, is_draft)\
        \ = (?, ?, ?, ?, ?) WHERE id = ?;"

updateComment
    :: Exception e
    => PSQL.Connection
    -> Comment
    -> IO (Either e (Maybe Comment))
updateComment conn Comment {..} = try $ do
    res <- PSQL.query conn q (commentText, commentText)
    case res of
        []        -> pure Nothing
        (obj : _) -> pure $ Just obj
    where q = "UPDATE post_comments SET (text) = (?) WHERE id = ?;"
