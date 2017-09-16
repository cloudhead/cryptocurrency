module Crypto.Blockchain.Hash where

import           Crypto.Hash (Digest, SHA256(..), digestFromByteString)
import qualified Crypto.Hash.Tree as HashTree
import           Data.Binary (Binary, get, put)
import           Data.ByteString hiding (putStrLn)
import           Data.ByteArray (convert)
import           Data.Maybe (fromJust)

instance Binary (Digest SHA256) where
    put digest =
        put (convert digest :: ByteString)
    get =
        fromJust . digestFromByteString <$> get @ByteString

instance Binary (HashTree.RootHash SHA256) where
    put (HashTree.RootHash n digest) =
        put n >> put digest
    get =
        HashTree.RootHash <$> get <*> get

