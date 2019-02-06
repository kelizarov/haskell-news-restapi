{-# LANGUAGE OverloadedStrings #-}

module Core.Monad.Logger (MonadLogger (..), L.withStdoutLogging, L.withFileLogging, L.flushLog) where

import           Data.Text
import qualified Control.Logging               as L

class MonadLogger m where
    logDebug :: Text -> m ()
    logInfo ::  Text -> m ()
    logWarn :: Text -> m ()
    logError :: Text -> m ()

performError :: Text -> Text -> IO ()
performError = L.loggingLogger L.LevelError

instance MonadLogger IO where
    logDebug = L.debug
    logInfo  = L.log
    logWarn  = L.warn
    logError = performError ""
