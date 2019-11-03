module News.UseCases.RegisterUser where

import qualified Data.Text                     as T

import qualified News.Models.Entity            as M
import qualified News.Models.User              as M

data Handle m = Handle
  { hPersistUser :: M.User -> m (M.ID M.User)
  , hLog :: String -> m ()
  }

data Result
  = UserCreated M.User
  | TransportError T.Text
  deriving (Show, Eq)

execute :: Monad m => Handle m -> T.Text -> T.Text -> m Result
execute Handle {..} firstname lastname = undefined
