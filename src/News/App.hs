module News.App
  ( runApp
  )
where

import qualified Data.ByteString.Char8         as BS
import           Network.HTTP.Types             ( statusCode )
import           Network.Wai                    ( Middleware
                                                , remoteHost
                                                , requestMethod
                                                , rawPathInfo
                                                , rawQueryString
                                                , responseStatus
                                                )
import           Network.Wai.Handler.Warp       ( run )

import           News.Env                       ( Env(..) )
import           News.Config                    ( AppConfig
                                                , loadConfig
                                                )

data AppHandle m = AppHandle
  { ahLogInfo :: String -> m ()
  , ahLogError :: String -> m ()
  , ahConfig :: AppConfig
  }

withLogging :: Middleware
withLogging app req respond = app req $ \response -> do
  putStrLn
    $  "["
    ++ clientIp req
    ++ "] "
    ++ method req
    ++ " "
    ++ statusOf response
    ++ ": "
    ++ query
  respond response
 where
  clientIp = show . remoteHost
  method   = show . requestMethod
  query    = BS.unpack $ BS.concat [rawPathInfo req, rawQueryString req]
  statusOf = show . statusCode . responseStatus

loadHandle :: Env -> IO (AppHandle IO)
loadHandle env = do
  config <- loadConfig env
  let logInfo  = putStrLn . (<>) "[INFO]:"
      logError = putStrLn . (<>) "[ERROR]:"
  pure $ AppHandle logInfo logError config

runApp :: Env -> IO ()
runApp env = do
  handle <- loadHandle env
  pure undefined
