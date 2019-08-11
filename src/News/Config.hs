module News.Config
  ( AppConfig(..)
  , loadConfig
  )
where

import qualified Data.Configurator             as C
import qualified Data.Configurator.Types       as C
import qualified Data.Text                     as T

import           News.Env                       ( Env(..) )

newtype Port =
  Port Int
  deriving (Show, Eq)

newtype Host =
  Host T.Text
  deriving (Show, Eq)

newtype Debug =
  Debug Bool
  deriving (Show, Eq)

data AppConfig = AppConfig
  { acEnv :: Env
  , acPort :: Port
  , acHost :: Host
  , acDebug :: Debug
  } deriving (Show, Eq)

loadConfig :: Env -> IO AppConfig
loadConfig env = do
  confFile <- loadConfigFile env
  port     <- C.require confFile "app.port"
  host     <- C.require confFile "app.host"
  debug    <- C.require confFile "app.debug"
  pure $ AppConfig env (Port port) (Host host) (Debug debug)

loadConfigFile :: Env -> IO C.Config
loadConfigFile env = do
  let path = case env of
        Prod -> "./config/prod.conf"
        Dev  -> "./config/dev.conf"
        Test -> "./config/test.conf"
  C.load [C.Required path]
