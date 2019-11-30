module News.UseCases.RegisterUser where

import qualified Data.Text as T

import qualified News.Models.Persisted as M
import qualified News.Models.User as M

data Handle m = Handle
  { hPersistUser :: T.Text -> T.Text -> Bool -> m (M.Persisted M.User)
  , hLog :: String -> m ()
  }

execute :: Monad m => Handle m -> T.Text -> T.Text -> m (M.Persisted M.User)
execute Handle {..} firstName lastName = do
  hLog $ "Registering new user: " <> show firstName <> ", " <> show lastName
  persistedUser <- hPersistUser firstName lastName False
  hLog $ "Successfully registered user: " <> show persistedUser
  pure persistedUser
