{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DeriveAnyClass #-}

module News.Endpoints.User where

import qualified Control.Exception             as EX
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Reader           ( asks )
import qualified Core.Config                   as C
import           Data.Aeson                     ( ToJSON(..) )
import qualified Data.Aeson                    as J
import qualified Data.Text                     as T
import qualified Data.Time                     as Time
import qualified Database.PostgreSQL.Simple    as PSQL
import           GHC.Generics
import           Network.Wai                    ( Request
                                                , strictRequestBody
                                                )

import           News.AppHandle
import           News.Config
import           News.Env
import qualified News.Models.Persisted            as M
import qualified News.Models.User              as M
import           News.Services.Database.Config
import           News.Services.Database.Queries.User
import qualified News.UseCases.RegisterUser    as RegisterUser

data Response a
  = Success a
  | InvalidRequest T.Text
  | TransportError
  deriving (Show, Eq)

instance (ToJSON a) => ToJSON (Response a) where
  toJSON (Success        a  ) = J.toJSON a
  toJSON (InvalidRequest err) = J.object ["error" J..= err]
  toJSON TransportError =
    J.object ["error" J..= ("Unexpected error happened!" :: String)]

data UserSerialized = UserSerialized
  { usId :: Int
  , usFirstName :: T.Text
  , usLastName :: T.Text
  , usAvatarPath :: T.Text
  , usCreatedAt :: Time.UTCTime
  , usIsAdmin :: Bool
  } deriving (Show, Eq, Generic, ToJSON)

createUserHandler :: Application (Response UserSerialized)
createUserHandler = do
  req  <- asks ahRequest
  conn <- asks ahConnection
  body <- liftIO $ strictRequestBody req
  liftIO $ print body
  let firstname   = "John"
      lastname    = "Doe"
      persistUser = liftIO . createUser conn
  res <- liftIO $ RegisterUser.execute
    (RegisterUser.Handle persistUser putStrLn)
    firstname
    lastname
  pure undefined

listUsersHandler :: MonadIO m => Request -> m [UserSerialized]
listUsersHandler = undefined

getUserByIdHandler :: MonadIO m => Request -> m (Maybe UserSerialized)
getUserByIdHandler = undefined
