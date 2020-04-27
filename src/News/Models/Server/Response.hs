module News.Models.Server.Response where

import Data.Text

type Reason = Text

data Response body 
  = OKResponse body
  | UnauthorizedRequest
  | InvalidRequest Reason
  | InternalError
  deriving (Show, Eq)