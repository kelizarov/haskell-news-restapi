{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.Monad.Handler where

import qualified Control.Exception as EX
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Fail
import qualified Core.Config as C
import qualified Core.Database as DB
import Core.Monad.Database
import Core.Monad.Logger
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PSQL
import Models.Post
import Network.Wai
import Text.Read

type Handler = MonadHandler Response

data HandlerEnv = HandlerEnv
  { hConfig :: C.Config
  , hPks :: [(T.Text, T.Text)]
  , hRequest :: Request
  , hConnection :: PSQL.Connection
  }

data HandlerError err
  = ParseError err
  | SQLError err
  | Forbidden
  deriving (Show)

newtype MonadHandler a = MonadHandler
  { runMonadHandler :: ExceptT (HandlerError String) (ReaderT HandlerEnv IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadFail
             , MonadReader HandlerEnv
             , MonadError (HandlerError String)
             )

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
  -> IO (Either (HandlerError String) a)
runHandler conf pks req conn = (`runReaderT` env) . runExceptT . runMonadHandler
  where
    env =
      HandlerEnv
        {hConfig = conf, hPks = pks, hRequest = req, hConnection = conn}
