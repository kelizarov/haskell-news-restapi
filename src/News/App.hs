module News.App
  ( runNews,
  )
where

import qualified Control.Exception as EX
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
  ( MonadReader,
    ReaderT,
    runReaderT,
  )
import qualified Data.ByteString.Char8 as BS
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Network.Wai as WAI
import Network.Wai.Handler.Warp (run)
import News.AppHandle
import News.Config
  ( AppConfig (..),
    loadConfig,
    loadConfigFile,
  )
import News.Env (Env (..))
import News.Server (route)
import News.Services.Database.Config (connect, connectInfo)

runNews :: IO ()
runNews = do
  conf@AppConfig {..} <- loadConfig Dev -- TODO read from system ENV
  putStrLn $ "Starting server on " <> show acPort <> " at " <> show acHost
  run acPort $ \request respond -> do
    dbConf <- loadConfigFile Dev
    dbConnectionInfo <- connectInfo dbConf
    EX.bracket (connect dbConnectionInfo) PSQL.close $ \conn -> do
      logRequest request
      response <- runApplication conf $ route request
      respond response

logRequest :: WAI.Request -> IO ()
logRequest request = putStrLn $ "Incoming request: " <> show request
