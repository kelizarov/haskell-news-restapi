module Core.Monad.Response where

import Network.Wai

class SerializedResponse m where
    respond :: m -> Response