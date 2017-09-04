module Crypto.Blockchain.Message where

import qualified Crypto.Blockchain as Blockchain
import           Data.Binary (Binary, get, put, encode)
import           GHC.Generics (Generic)

data Message a =
      Tx a
    | Block (Blockchain.Block a)
    | Ping
    deriving (Show, Generic)

instance Binary a => Binary (Message a)
deriving instance Eq a => Eq (Message a)

