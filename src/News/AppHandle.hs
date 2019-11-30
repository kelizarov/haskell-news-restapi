{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module News.AppHandle where

import qualified Control.Exception as EX
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import qualified Data.ByteString.Char8 as BS
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Network.Wai as WAI
import Network.Wai.Handler.Warp (run)

import News.Config

data AppEnv = AppEnv
  { ahConfig :: AppConfig
  , ahRequest :: WAI.Request
  , ahConnection :: PSQL.Connection
  }

newtype Application a = Application
  { runApp :: ReaderT AppEnv IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadFail
             , MonadIO
             , MonadReader AppEnv
             )

runApplication ::
     AppConfig -> WAI.Request -> PSQL.Connection -> Application a -> IO a
runApplication conf@AppConfig {..} request connection =
  (`runReaderT` env) . runApp
  where
    env = AppEnv conf request connection
