{-# LANGUAGE OverloadedStrings #-}

module RouterSpec
  ( spec
  ) where

import Data.ByteString.Builder (Builder, toLazyByteString, word8)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.IORef
import Network.HTTP.Types as HTTP
import Network.Wai
import Test.Hspec

import API.Handlers (responseError, responseOk)
import qualified API.Router as R
import qualified Core.Config as C
import Core.Monad.Handler

spec :: Spec
spec = undefined
  -- describe "traverseRoute" $ do
  --   it "should route to root with GET method" $ do
  --     let res =
  --           R.traverseRoute
  --             (defaultRequest {requestMethod = "GET"})
  --             []
  --             methodGet
  --     res `shouldBe` (True, [])
  --   it "should not route to root if wrong method" $ do
  --     let res =
  --           R.traverseRoute
  --             (defaultRequest {requestMethod = "POST"})
  --             []
  --             methodGet
  --     res `shouldBe` (False, [])
  --   it "should not route to root if url is wrong" $ do
  --     let res =
  --           R.traverseRoute
  --             (defaultRequest {requestMethod = "GET", pathInfo = ["api"]})
  --             []
  --             methodGet
  --     res `shouldBe` (False, [])
  --   it "should route to GET/POST/PATCH routes" $ do
  --     let post =
  --           R.traverseRoute
  --             (defaultRequest {pathInfo = ["api"], requestMethod = "POST"})
  --             ["api"]
  --             methodPost
  --     let patch =
  --           R.traverseRoute
  --             (defaultRequest {pathInfo = ["api", "0"], requestMethod = "PATCH"})
  --             ["api", ":pk"]
  --             methodPatch
  --     let get =
  --           R.traverseRoute
  --             (defaultRequest {pathInfo = ["api", "0"], requestMethod = "GET"})
  --             ["api", ":pk"]
  --             methodGet
  --     let list =
  --           R.traverseRoute
  --             (defaultRequest {pathInfo = ["api"], requestMethod = "GET"})
  --             ["api"]
  --             methodGet
  --     post `shouldBe` (True, [])
  --     patch `shouldBe` (True, [("pk", "0")])
  --     get `shouldBe` (True, [("pk", "0")])
  --     list `shouldBe` (True, [])
  -- describe "route" $ do
  --   let rootListHandler :: Handler
  --       rootListHandler = responseOk "list"
  --       rootRetrieveHandler :: Handler
  --       rootRetrieveHandler = responseOk "get"
  --       rootCreateHandler :: Handler
  --       rootCreateHandler = responseOk "post"
  --       rootUpdateHandler :: Handler
  --       rootUpdateHandler = responseOk "patch"
  --       routes =
  --         [ (["api"], methodGet, rootListHandler)
  --         , (["api", ":pk"], methodGet, rootRetrieveHandler)
  --         , (["api"], methodPost, rootCreateHandler)
  --         , (["api", ":pk"], methodPatch, rootUpdateHandler)
  --         ]
  --       getBody res = do
  --         let (_, _, f) = responseToStream res
  --         f $ \streamingBody -> do
  --           builderRef <- newIORef mempty
  --           let add :: Builder -> IO ()
  --               add b =
  --                 atomicModifyIORef builderRef $ \builder ->
  --                   (builder `mappend` b, ())
  --               flush :: IO ()
  --               flush = return ()
  --           streamingBody add flush
  --           fmap (LBS.toStrict . toLazyByteString) $ readIORef builderRef
  --   it "should call right handler for each route" $ do
  --     conf <- C.loadConfig
  --     list <-
  --       R.route
  --         conf
  --         routes
  --         (defaultRequest {pathInfo = ["api"], requestMethod = "GET"})
  --     get <-
  --       R.route
  --         conf
  --         routes
  --         (defaultRequest {pathInfo = ["api", "0"], requestMethod = "GET"})
  --     post <-
  --       R.route
  --         conf
  --         routes
  --         (defaultRequest {pathInfo = ["api"], requestMethod = "POST"})
  --     patch <-
  --       R.route
  --         conf
  --         routes
  --         (defaultRequest {pathInfo = ["api", "0"], requestMethod = "PATCH"})
  --     getBody list `shouldReturn` "list"
  --     getBody get `shouldReturn` "get"
  --     getBody post `shouldReturn` "post"
  --     getBody patch `shouldReturn` "patch"
