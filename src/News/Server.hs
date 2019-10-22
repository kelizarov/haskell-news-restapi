module News.Server where

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
