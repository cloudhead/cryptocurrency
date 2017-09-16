module Crypto.Blockchain.Block where

import           Crypto.Blockchain.Hash ()
import           Crypto.Blockchain.Types

import           Data.Binary (Binary, encode)
import           Data.Sequence (Seq)
import           Crypto.Hash (Digest, SHA256(..), HashAlgorithm, hashlazy, digestFromByteString, hashDigestSize)
import qualified Crypto.Hash.Tree as HashTree
import           Crypto.Number.Serialize (os2ip)
import           Data.Word (Word32)
import           Data.ByteString hiding (putStrLn)
import           Data.ByteArray (zero)
import           Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)

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

data Block tx = Block
    { blockHeader :: BlockHeader
    , blockData   :: Seq tx
    } deriving (Show, Generic)

instance (Binary a) => Binary (Block a)
deriving instance Eq a => Eq (Block a)

instance Validate (Block a) where
    validate = validateBlock

validateBlock :: Block a -> Either Error (Block a)
validateBlock blk = Right blk

difficulty :: BlockHeader -> Difficulty
difficulty bh = os2ip (hashlazy $ encode bh :: Digest SHA256)

zeroHash :: HashAlgorithm a => Digest a
zeroHash = fromJust $
    digestFromByteString (zero (hashDigestSize SHA256) :: ByteString)

block :: [a] -> Either Error (Block a)
block xs = validate $
    Block
        BlockHeader
            { blockPreviousHash = zeroHash
            , blockDifficulty   = undefined
            , blockTimestamp    = undefined
            , blockRootHash     = undefined
            , blockNonce        = undefined
            }
        (Seq.fromList xs)

genesisDifficulty :: Difficulty
genesisDifficulty = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

genesisBlock :: [a] -> Either Error (Block a)
genesisBlock xs = validate $
    Block
        BlockHeader
            { blockPreviousHash = zeroHash
            , blockDifficulty   = genesisDifficulty
            , blockTimestamp    = undefined
            , blockRootHash     = undefined
            , blockNonce        = 0
            }
        (Seq.fromList xs)

isGenesisBlock :: Block a -> Bool
isGenesisBlock blk =
    (blockPreviousHash . blockHeader) blk == zeroHash

