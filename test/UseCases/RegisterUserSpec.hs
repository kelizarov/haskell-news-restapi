module UseCases.RegisterUserSpec where

import           Test.Hspec
import           Control.Monad.State            ( State )

import qualified News.UseCases.RegisterUser    as UseCase
import qualified News.Models.Entity            as M

handle = UseCase.Handle
  { UseCase.hSaveUser = \user -> pure $ M.Entity (M.ID 1) user
  , UseCase.hLog      = \msg -> pure ()
  }

spec :: Spec
spec = describe "RegisterUser" $ do
  it "Should register with standard data" $ do
    let firstName = "John"
        lastName  = "Doe"
    res <- UseCase.execute handle firstName lastName
    res `shouldBe` UseCase.UserCreated
  it "Should fail if error in transport occurred" $ do
    let firstName = "John"
        lastName  = "Doe"
        handle'   = handle
    res <- UseCase.execute handle' firstName lastName
    res `shouldBe` UseCase.TransportError "Error happened in transport"
