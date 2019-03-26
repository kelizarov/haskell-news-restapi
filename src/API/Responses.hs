{-# LANGUAGE OverloadedStrings #-}

module API.Responses where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.HTTP.Types as HTTP
import Network.Wai

responseOk :: Monad m => LBS.ByteString -> m Response
responseOk b =
  pure $ responseLBS HTTP.status200 [(HTTP.hContentType, "text/plain")] b

responseError :: Applicative m => LBS.ByteString -> m Response
responseError b =
  pure $ responseLBS HTTP.status400 [(HTTP.hContentType, "text/plain")] b
