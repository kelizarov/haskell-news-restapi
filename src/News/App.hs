module News.App
  ( runApp
  )
where

import qualified Data.ByteString.Char8         as BS

import           News.Config                    ( AppConfig
                                                , loadConfig
                                                )
import           News.Env                       ( Env(..) )

newtype AppHandle = AppHandle
  { ahConfig :: AppConfig
  } deriving (Show, Eq)

loadAppHandle :: Env -> IO AppHandle
loadAppHandle env = AppHandle <$> loadConfig env

runApp :: Env -> IO ()
runApp env = do
  handle <- loadAppHandle env
  putStrLn "App is not implemented yet"
  pure ()
