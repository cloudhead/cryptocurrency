module Crypto.Blockchain.Block where

import           Crypto.Blockchain.Hash ()

import           Data.Binary (Binary, encode)
import           Data.Sequence (Seq)
import           Crypto.Hash (Digest, SHA256(..), HashAlgorithm, hashlazy, digestFromByteString, hashDigestSize)
import qualified Crypto.Hash.Tree as HashTree
import           Crypto.Number.Serialize (os2ip)
import           Data.Word (Word32)
import           Data.ByteString hiding (putStrLn)
import           Data.ByteArray (zero)
import           Data.Maybe (fromJust)
import           GHC.Generics (Generic)

type Block' tx = Block tx
type Difficulty = Integer
type Timestamp = Word32

data BlockHeader = BlockHeader
    { blockPreviousHash :: Digest SHA256
    , blockRootHash     :: HashTree.RootHash SHA256
    , blockNonce        :: Word32
    , blockDifficulty   :: Difficulty
    , blockTimestamp    :: Timestamp
    } deriving (Show, Generic)

instance Eq BlockHeader where
    (==) _h _h' = undefined

emptyBlockHeader :: BlockHeader
emptyBlockHeader = BlockHeader
    { blockPreviousHash = zeroHash
    , blockRootHash = (HashTree.RootHash 0 zeroHash)
    , blockNonce = 0
    , blockDifficulty = 0
    , blockTimestamp = 0
    }

instance Binary BlockHeader

data Block a = Block
    { blockHeader :: BlockHeader
    , blockData   :: Seq a
    } deriving (Show, Generic)

instance (Binary a) => Binary (Block a)
deriving instance Eq a => Eq (Block a)

difficulty :: BlockHeader -> Difficulty
difficulty bh = os2ip (hashlazy $ encode bh :: Digest SHA256)

zeroHash :: HashAlgorithm a => Digest a
zeroHash = fromJust $
    digestFromByteString (zero (hashDigestSize SHA256) :: ByteString)

