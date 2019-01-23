{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.Wai
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp       ( run )
import qualified Data.ByteString.Char8         as BS

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

application :: Application
application _ respond = respond
    $ responseLBS status200 [(hContentType, "text/plain")] "Hello, world!"

main :: IO ()
main = do
    putStrLn "Serving at http://localhost:8000"
    run 8000 $ withLogging application
