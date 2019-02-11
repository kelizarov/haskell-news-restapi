module Playground where

import Data.Functor.Identity (Identity(..))
import Prelude hiding (Either(..))

data Either l r
  = Left l
  | Right r
  deriving (Show)

instance Functor (Either a) where
  fmap f (Right a) = Right $ f a
  fmap _ (Left a) = Left a

instance Applicative (Either a) where
  pure = Right
  Right f <*> Right a = Right $ f a
  Left b <*> _ = Left b
  _ <*> Left b = Left b

instance Monad (Either a) where
  return = Right
  (>>=) (Right a) f = f a
  (>>=) (Left a) _ = Left a

newtype ExceptT e m a = ExceptT
  { runExceptT :: m (Either e a)
  }

type Except e a = ExceptT e Identity a

instance (Functor m) => Functor (ExceptT e m) where
  fmap f = ExceptT . fmap (fmap f) . runExceptT

instance (Applicative m, Monad m) => Applicative (ExceptT e m) where
  pure = ExceptT . pure . pure
  (ExceptT mfab) <*> (ExceptT mfa) =
    ExceptT $ do
      fab <- mfab
      fa <- mfa
      pure $ fab <*> fa

type MyMonad a = ExceptT String IO a

type Handler = MyMonad ()
-- testEitherMonand :: Except [Char] Integer
-- testEitherMonand = do
--     a <- Right 3
--     b <- Left "error"
--     return $ a * b
