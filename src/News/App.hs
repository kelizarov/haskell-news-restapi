module News.App
  ( app
  )
where

import qualified Data.ByteString.Char8         as BS
import           Network.Wai
import           Network.Wai.Handler.Warp       ( run )
import           Network.HTTP.Types

import           News.Config                    ( AppConfig(..)
                                                , loadConfig
                                                )
import           News.Server                    ( api )
import           News.Env                       ( Env(..) )

newtype AppHandle = AppHandle
  { ahConfig :: AppConfig
  } deriving (Show, Eq)

loadAppHandle :: Env -> IO AppHandle
loadAppHandle env = AppHandle <$> loadConfig env

runApp :: AppHandle -> Application
runApp AppHandle {..} request respond = do
  -- TODO do handling
  respond undefined

app :: Env -> IO ()
app env = do
  ah@AppHandle {..} <- loadAppHandle env
  run (acPort ahConfig) $ runApp ah
