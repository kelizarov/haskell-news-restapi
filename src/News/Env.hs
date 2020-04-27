module News.Env
  ( Env (..),
  )
where

data Env
  = Prod
  | Dev
  | Test
  deriving (Eq)

instance Show Env where
  show Prod = "prod"
  show Dev = "dev"
  show Test = "test"

instance Read Env where
  readsPrec _ "prod" = [(Prod, "")]
  readsPrec _ "dev" = [(Dev, "")]
  readsPrec _ "test" = [(Test, "")]
