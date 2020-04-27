{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module News.Endpoints.User where

import qualified Control.Exception as EX
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson ((.:), FromJSON (..), ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Database.PostgreSQL.Simple as PSQL
import GHC.Generics (Generic)
import Network.Wai (Request, strictRequestBody)
import News.AppHandle
import News.Config
import News.Env
import qualified News.Models.Persisted as M
import qualified News.Models.Server.Response as API
import qualified News.Models.User as M
import News.Services.Database.Implementation.PostgresSimple
import qualified News.Services.Database.Query.User as Query
import qualified News.UseCases.RegisterUser as RegisterUser

instance (ToJSON resp) => ToJSON (API.Response resp) where
  toJSON (API.OKResponse resp) = J.toJSON resp
  toJSON (API.InvalidRequest err) = J.object ["error" J..= err]
  toJSON API.InternalError =
    J.object ["error" J..= ("Unexpected error happened!" :: String)]

data UserSerialized
  = UserSerialized
      { usId :: Int,
        usFirstName :: T.Text,
        usLastName :: T.Text,
        usAvatarPath :: T.Text,
        usCreatedAt :: Time.UTCTime,
        usIsAdmin :: Bool
      }
  deriving (Show, Eq, Generic)

serializePersistedUser :: M.Persisted M.User -> UserSerialized
serializePersistedUser persistedUser =
  let userId = M.getId persistedUser
      M.User {..} = M.getObj persistedUser
   in UserSerialized
        { usId = M.fromID userId,
          usFirstName = uFirstName,
          usLastName = uLastName,
          usAvatarPath = uAvatarPath,
          usCreatedAt = uCreatedAt,
          usIsAdmin = uIsAdmin
        }

data CreateUserRequest
  = CreateUserRequest
      { curFirstName :: T.Text,
        curLastName :: T.Text
      }
  deriving (Show, Eq)

instance FromJSON CreateUserRequest where
  parseJSON =
    J.withObject "CreateUserRequest" $ \o ->
      CreateUserRequest <$> o .: "first_name" <*> o .: "last_name"

createUserEndpoint :: CreateUserRequest -> Application (API.Response UserSerialized)
createUserEndpoint CreateUserRequest {..} = do
  let hPersistUser = Query.createUser
  let hLog = liftIO . putStrLn
  persistedUser <-
    RegisterUser.execute RegisterUser.Handle {..} curFirstName curLastName
  pure . API.OKResponse $ serializePersistedUser persistedUser

listUsersEndpoint :: Request -> Application [UserSerialized]
listUsersEndpoint = undefined

getUserByIdEndpoint :: Request -> Application (Maybe UserSerialized)
getUserByIdEndpoint = undefined
