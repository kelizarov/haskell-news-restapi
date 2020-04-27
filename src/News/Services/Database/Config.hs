module News.Services.Database.Config where

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Database.PostgreSQL.Simple as PSQL

connectInfo :: C.Config -> IO PSQL.ConnectInfo
connectInfo conf = do
  host <- C.require conf "database.host"
  port <- C.require conf "database.port"
  user <- C.require conf "database.user"
  password <- C.require conf "database.password"
  database <- C.require conf "database.database"
  pure $
    PSQL.ConnectInfo
      { PSQL.connectHost = host,
        PSQL.connectPort = port,
        PSQL.connectUser = user,
        PSQL.connectPassword = password,
        PSQL.connectDatabase = database
      }

connect :: PSQL.ConnectInfo -> IO PSQL.Connection
connect = PSQL.connect
