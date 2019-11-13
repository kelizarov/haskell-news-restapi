module News.App
  ( app
  )
where

import qualified Control.Exception             as EX
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , runReaderT
                                                )
import qualified Data.ByteString.Char8         as BS
import qualified Database.PostgreSQL.Simple    as PSQL
import qualified Network.Wai                   as WAI
import           Network.Wai.Handler.Warp       ( run )

import           News.Config                    ( AppConfig(..)
                                                , loadConfig
                                                , loadConfigFile
                                                )
import           News.Env                       ( Env(..) )
import           News.AppHandle
import           News.Server                    ( route )
import           News.Services.Database.Config  ( connect )

app :: Env -> IO ()
app env = do
  conf@AppConfig {..} <- loadConfig env
  dbConf              <- loadConfigFile env
  run acPort $ \request respond ->
    EX.bracket (connect dbConf) PSQL.close $ \conn -> do
      response <- runApplication conf request conn (route request)
      respond response
