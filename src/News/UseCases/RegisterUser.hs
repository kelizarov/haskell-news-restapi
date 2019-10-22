module News.UseCases.RegisterUser where

import qualified Data.Text                     as T

import qualified News.Models.Entity            as Model
import qualified News.Models.User              as Model

data Handle m = Handle
  { hSaveUser :: Model.User -> m (Model.Entity Model.User)
  , hLog :: String -> m ()
  }

data Result
  = UserCreated
  | TransportError T.Text
  deriving (Show, Eq)

execute :: Monad m => Handle m -> T.Text -> T.Text -> m Result
execute Handle {..} firstname lastname = undefined
