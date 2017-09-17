module Crypto.Blockchain.Types where

import           Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Binary as Binary
import           Data.Binary (Binary)
import qualified Data.ByteString.Lazy as LBS

newtype Error = Error String

class Validate a where
    -- TODO: Consider making this
    -- a -> Either Error (Valid a)
    validate :: a -> Either Error a

newtype Hashed a d = Hashed (Digest d)
    deriving (Eq, Ord)

hashed :: Binary a => a -> Hashed a SHA256
hashed a =
    Hashed . hash . LBS.toStrict $ Binary.encode a
