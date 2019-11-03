module News.Endpoints.User where

import qualified Control.Exception             as EX
import           Control.Monad.Except
import           Control.Monad.IO.Class
import qualified Core.Config                   as C
import           Data.Aeson
import           Data.Text
import qualified Database.PostgreSQL.Simple    as PSQL
import           Network.Wai

import qualified News.Models.Entity            as M
import qualified News.Models.User              as M
import qualified News.UseCases.RegisterUser    as UseCase
import           News.Config
import           News.Env

-- TODO move to DB layer
connectInfo :: C.Config -> IO PSQL.ConnectInfo
connectInfo conf = do
  host     <- C.get conf "database.host"
  port     <- C.get conf "database.port"
  user     <- C.get conf "database.user"
  password <- C.get conf "database.password"
  database <- C.get conf "database.database"
  pure $ PSQL.ConnectInfo { PSQL.connectHost     = host
                          , PSQL.connectPort     = port
                          , PSQL.connectUser     = user
                          , PSQL.connectPassword = password
                          , PSQL.connectDatabase = database
                          }

connect :: C.Config -> IO PSQL.Connection
connect conf = connectInfo conf >>= PSQL.connect

createUser :: PSQL.Connection -> M.User -> IO (M.ID M.User)
createUser conn M.User {..} = do
  (M.PersistedUser entity : _) <-
    PSQL.query conn q (uFirstName, uLastName, uIsAdmin) :: IO [M.PersistedUser]
  pure $ M.getId entity
 where
  q
    = "INSERT INTO users (first_name, last_name, is_admin, created_on) \
        \VALUES (?, ?, ?, CURRENT_TIMESTAMP) RETURNING *;"

createUserHandler :: C.Config -> Request -> IO (IO UseCase.Result)
createUserHandler conf req = EX.bracket (connect conf) PSQL.close $ \conn -> do
  body <- strictRequestBody req
  print body
  let firstname   = "John"
      lastname    = "Doe"
      persistUser = createUser conn
  pure
    $ UseCase.execute (UseCase.Handle persistUser putStrLn) firstname lastname
