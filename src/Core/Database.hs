{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Core.Database where

import           Database.PostgreSQL.Simple.Migration
import qualified Database.PostgreSQL.Simple    as PSQL
import           Data.Text
import           Data.Proxy
import           Control.Exception
import           Control.Monad.IO.Class

import qualified Core.Config                   as C

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

class Persistent a where
    tableName :: Proxy a -> PSQL.Query
    update :: PSQL.Connection -> a -> IO a
    insert :: PSQL.Connection -> a -> IO a
    list :: PSQL.Connection -> (Int, Int) -> IO [a]
    select :: PSQL.Connection -> Int -> IO (Maybe a)

    default list :: (PSQL.FromRow a) => PSQL.Connection -> (Int, Int) -> IO [a]
    list conn (limit, offset) = PSQL.query conn q (limit, offset)
        where
            q = "SELECT * FROM "
                <> tableName (Proxy :: Proxy a)
                <> " LIMIT ? OFFSET ?;"

    default select :: (PSQL.FromRow a) => PSQL.Connection -> Int -> IO (Maybe a)
    select conn id = do
        res <- PSQL.query conn q [id]
        case res of
            [] -> pure Nothing
            (obj: _) -> pure $ Just obj
        where
            q = "SELECT * FROM "
                <> tableName (Proxy :: Proxy a)
                <> " WHERE id = ?;"
