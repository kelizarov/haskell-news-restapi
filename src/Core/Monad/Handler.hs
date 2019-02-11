{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Monad.Handler where

import qualified Control.Exception as EX
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Core.Config as C
import qualified Core.Database as DB
import Core.Monad.Logger
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PSQL
import Models.Post
import Models.User
import Network.Wai
import Text.Read

type Handler = MonadHandler Response

data HandlerEnv = HandlerEnv
  { hConfig :: C.Config
  , hPks :: [(T.Text, T.Text)]
  , hRequest :: Request
  , hConnection :: PSQL.Connection
  }

newtype MonadHandler a = MonadHandler
  { runMonadHandler :: ReaderT HandlerEnv IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader HandlerEnv)

instance MonadLogger MonadHandler where
  logDebug = liftIO . logDebug
  logInfo = liftIO . logInfo
  logWarn = liftIO . logWarn
  logError = liftIO . logError

runHandler ::
     C.Config
  -> [(T.Text, T.Text)]
  -> Request
  -> PSQL.Connection
  -> MonadHandler a
  -> IO a
runHandler conf pks req conn = (`runReaderT` env) . runMonadHandler
  where
    env =
      HandlerEnv
        {hConfig = conf, hPks = pks, hRequest = req, hConnection = conn}

getRequestUser :: MonadHandler (Maybe User)
getRequestUser = do
  conn <- asks hConnection
  req <- asks hRequest
  pks <- asks hPks
  liftIO $ getUser req conn

getUser :: Request -> PSQL.Connection -> IO (Maybe User)
getUser req conn =
  case lookup "Authorization" (requestHeaders req) >>= (readMaybe . BS.unpack) of
    Nothing -> pure Nothing
    Just uId -> do
      res <- EX.try $ DB.select conn uId
      either error success res
      where error :: EX.SomeException -> IO (Maybe User)
            error _ = pure Nothing
            success = pure
