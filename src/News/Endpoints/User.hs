{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DeriveAnyClass #-}

module News.Endpoints.User where

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:))
import GHC.Generics (Generic)
import Network.Wai (Request, strictRequestBody)

import qualified Control.Exception as EX
import qualified Core.Config as C
import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Database.PostgreSQL.Simple as PSQL

import News.AppHandle
import News.Config
import News.Env

import qualified News.Models.Persisted as M
import qualified News.Models.User as M
import qualified News.Services.Database.Queries.User as DB
import qualified News.UseCases.RegisterUser as RegisterUser

data Response a
  = Success a
  | InvalidRequest T.Text
  | InternalError
  deriving (Show, Eq)

instance (ToJSON a) => ToJSON (Response a) where
  toJSON (Success a) = J.toJSON a
  toJSON (InvalidRequest err) = J.object ["error" J..= err]
  toJSON InternalError =
    J.object ["error" J..= ("Unexpected error happened!" :: String)]

data UserSerialized = UserSerialized
  { usId :: Int
  , usFirstName :: T.Text
  , usLastName :: T.Text
  , usAvatarPath :: T.Text
  , usCreatedAt :: Time.UTCTime
  , usIsAdmin :: Bool
  } deriving (Show, Eq, Generic)

serializePersistedUser :: M.Persisted M.User -> UserSerialized
serializePersistedUser persistedUser =
  let userId = M.getId persistedUser
      M.User {..} = M.getObj persistedUser
   in UserSerialized
        { usId = M.fromID userId
        , usFirstName = uFirstName
        , usLastName = uLastName
        , usAvatarPath = uAvatarPath
        , usCreatedAt = uCreatedAt
        , usIsAdmin = uIsAdmin
        }

data CreateUserRequest = CreateUserRequest
  { curFirstName :: T.Text
  , curLastName :: T.Text
  } deriving (Show, Eq)

instance FromJSON CreateUserRequest where
  parseJSON =
    J.withObject "CreateUserRequest" $ \o ->
      CreateUserRequest <$> o .: "first_name" <*> o .: "last_name"

-- TODO add exception handling of some sorts
createUserEndpoint :: CreateUserRequest -> Application (Response UserSerialized)
createUserEndpoint CreateUserRequest {..} = do
  let hPersistUser = DB.createUser
  let hLog = liftIO . putStrLn
  persistedUser <-
    RegisterUser.execute RegisterUser.Handle {..} curFirstName curLastName
  pure . Success $ serializePersistedUser persistedUser

listUsersEndpoint :: MonadIO m => Request -> m [UserSerialized]
listUsersEndpoint = undefined

getUserByIdEndpoint :: MonadIO m => Request -> m (Maybe UserSerialized)
getUserByIdEndpoint = undefined
