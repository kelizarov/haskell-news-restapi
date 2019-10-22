{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module News.API
  ( api
  )
where

import           Data.Text
import           Network.Wai

import           News.Services.Server

api :: Request -> IO Response
api req = case methodAndPath req of
  POST (matches ["api", "user"] -> Just []) -> undefined
  GET (matches ["api", "user", ":pk"] -> Just [userId]) -> undefined
  UNKNOWN -> undefined
