module Core.Config
    ( loadConfig
    , get
    , C.Config
    )
where

import qualified Data.Configurator             as C
import qualified Data.Configurator.Types       as C

loadConfig :: IO C.Config
loadConfig = fst <$> C.autoReload C.autoConfig [C.Required "conf/local.conf"]

get :: C.Configured a => C.Config -> C.Name -> IO a
get = C.require