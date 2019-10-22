module News.Config
  ( AppConfig(..)
  , loadConfig
  )
where

import qualified Data.Configurator             as C
import qualified Data.Configurator.Types       as C
import qualified Data.Text                     as T

import           News.Env                       ( Env(..) )

type Port = Int

type Host = T.Text

type Debug = Bool

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
  pure $ AppConfig env port host debug

loadConfigFile :: Env -> IO C.Config
loadConfigFile env = do
  let path = case env of
        Prod -> "./config/prod.conf"
        Dev  -> "./config/dev.conf"
        Test -> "./config/test.conf"
  C.load [C.Required path]
