module Core.Exceptions where

data HandlerError err
  = ParseError err
  | SQLError err
  | Forbidden
  | Unathuorized
  deriving (Show)