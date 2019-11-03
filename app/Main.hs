{-# LANGUAGE OverloadedStrings #-}

module Main where

import           API.Router                     ( route
                                                , routeTable
                                                )
import qualified Core.Config                   as C
import qualified Core.Monad.Logger             as L
import qualified Data.ByteString.Char8         as BS
import           Data.Text
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp       ( run )

import qualified News.App                      as App
import qualified News.Env                      as App

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
main = App.app App.Dev
