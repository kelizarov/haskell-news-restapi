{-# LANGUAGE OverloadedStrings #-}
module RouterSpec
    ( spec
    )
where

import           Test.Hspec
import           Network.Wai
import           Network.HTTP.Types            as HTTP
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Data.ByteString.Builder        ( Builder
                                                , toLazyByteString
                                                , word8
                                                )
import           Data.IORef

import           Core.Monad.Handler
import qualified Core.Config                   as C
import           API.Handlers                   ( responseOk
                                                , responseError
                                                )
import qualified API.Router                    as R

rootRoute :: R.Route
rootRoute = R.MethodRoute "GET"

listRootRoute :: R.Route
listRootRoute = R.PathRoute "api" $ R.MethodRoute "GET"

retrieveRootRoute :: R.Route
retrieveRootRoute =
    R.PathRoute "api" $ R.DynamicRoute "pk" $ R.MethodRoute "GET"

createRootRoute :: R.Route
createRootRoute = R.PathRoute "api" $ R.MethodRoute "POST"

updateRootRoute :: R.Route
updateRootRoute =
    R.PathRoute "api" $ R.DynamicRoute "pk" $ R.MethodRoute "PATCH"

spec :: Spec
spec = do
    describe "traverseRoute" $ do
        it "should route to root with GET method" $ do
            let
                res = R.traverseRoute
                    (defaultRequest { requestMethod = "GET" })
                    rootRoute
            res `shouldBe` (True, [])
        it "should not route to root if wrong method" $ do
            let
                res = R.traverseRoute
                    (defaultRequest { requestMethod = "POST" })
                    rootRoute
            res `shouldBe` (False, [])
        it "should not route to root if url is wrong" $ do
            let res = R.traverseRoute
                    (defaultRequest { requestMethod = "GET"
                                    , pathInfo      = ["api"]
                                    }
                    )
                    rootRoute
            res `shouldBe` (False, [])
        it "should route to GET/POST/PATCH routes" $ do
            let post = R.traverseRoute
                    (defaultRequest { pathInfo      = ["api"]
                                    , requestMethod = "POST"
                                    }
                    )
                    createRootRoute
            let patch = R.traverseRoute
                    (defaultRequest { pathInfo      = ["api", "0"]
                                    , requestMethod = "PATCH"
                                    }
                    )
                    updateRootRoute
            let get = R.traverseRoute
                    (defaultRequest { pathInfo      = ["api", "0"]
                                    , requestMethod = "GET"
                                    }
                    )
                    retrieveRootRoute
            let list = R.traverseRoute
                    (defaultRequest { pathInfo      = ["api"]
                                    , requestMethod = "GET"
                                    }
                    )
                    listRootRoute
            post `shouldBe` (True, [])
            patch `shouldBe` (True, [("pk", "0")])
            get `shouldBe` (True, [("pk", "0")])
            list `shouldBe` (True, [])
    describe "route" $ do
        let rootListHandler :: Handler
            rootListHandler = responseOk "list"

            rootRetrieveHandler :: Handler
            rootRetrieveHandler = responseOk "get"

            rootCreateHandler :: Handler
            rootCreateHandler = responseOk "post"

            rootUpdateHandler :: Handler
            rootUpdateHandler = responseOk "patch"

            routes =
                [ (listRootRoute    , rootListHandler)
                , (retrieveRootRoute, rootRetrieveHandler)
                , (createRootRoute  , rootCreateHandler)
                , (updateRootRoute  , rootUpdateHandler)
                ]

            getBody res = do
                let (_, _, f) = responseToStream res
                f $ \streamingBody -> do
                    builderRef <- newIORef mempty
                    let add :: Builder -> IO ()
                        add b = atomicModifyIORef builderRef
                            $ \builder -> (builder `mappend` b, ())
                        flush :: IO ()
                        flush = return ()
                    streamingBody add flush
                    fmap (LBS.toStrict . toLazyByteString)
                        $ readIORef builderRef
        it "should call right handler for each route" $ do
            conf <- C.loadConfig
            list <- R.route
                conf
                routes
                (defaultRequest { pathInfo = ["api"], requestMethod = "GET" })
            get <- R.route
                conf
                routes
                (defaultRequest { pathInfo      = ["api", "0"]
                                , requestMethod = "GET"
                                }
                )
            post <- R.route
                conf
                routes
                (defaultRequest { pathInfo = ["api"], requestMethod = "POST" })
            patch <- R.route
                conf
                routes
                (defaultRequest { pathInfo      = ["api", "0"]
                                , requestMethod = "PATCH"
                                }
                )
            getBody list `shouldReturn` "list"
            getBody get `shouldReturn` "get"
            getBody post `shouldReturn` "post"
            getBody patch `shouldReturn` "patch"
    -- describe "handler" $ do
    --     it "should route to handle with no pk" $ do
    --         undefined
    --     it "should route to handle with pk" $ do
    --         undefined
    --     it "should route to handler with limit and offset on list" $ do
    --         undefined






