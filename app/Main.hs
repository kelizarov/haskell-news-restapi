{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.Wai
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp       ( run )
import qualified Data.ByteString.Char8         as BS
import API.Router (route, routeTable)
import qualified Core.Config as C

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

application :: C.Config -> Application
application conf req respond = route conf routeTable req >>= respond

main :: IO ()
main = do
    putStrLn "Serving at http://localhost:8000"
    conf <- C.loadConfig
    run 8000 $ withLogging (application conf)
