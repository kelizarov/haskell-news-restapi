{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Core.Database where

import Control.Exception
import Control.Monad.IO.Class
import qualified Core.Config as C
import Data.Proxy
import Data.Text
import qualified Database.PostgreSQL.Simple as PSQL
import Database.PostgreSQL.Simple.Migration

connectInfo :: C.Config -> IO PSQL.ConnectInfo
connectInfo conf = do
  host <- C.get conf "database.host"
  port <- C.get conf "database.port"
  user <- C.get conf "database.user"
  password <- C.get conf "database.password"
  database <- C.get conf "database.database"
  pure $
    PSQL.ConnectInfo
      { PSQL.connectHost = host
      , PSQL.connectPort = port
      , PSQL.connectUser = user
      , PSQL.connectPassword = password
      , PSQL.connectDatabase = database
      }

connect :: C.Config -> IO PSQL.Connection
connect conf = connectInfo conf >>= PSQL.connect

migrate :: IO ()
migrate =
  bracket (C.loadConfig >>= connect) PSQL.close $ \conn -> do
    result <- PSQL.withTransaction conn (runMigrations False conn cmds)
    case result of
      MigrationError err -> error err
      _ -> return ()
  where
    cmds = [MigrationInitialization, MigrationDirectory "./migrations"]
