module Crypto.Blockchain.Block where

import           Crypto.Blockchain.Hash ()
import           Crypto.Blockchain.Types

import           Data.Binary (Binary, encode)
import           Data.Sequence (Seq)
import           Data.Foldable (toList)
import           Crypto.Hash (Digest, SHA256(..), HashAlgorithm, hashlazy, digestFromByteString, hashDigestSize)
import           Crypto.Number.Serialize (os2ip)
import qualified Crypto.Hash.MerkleTree as Merkle
import           Data.Word (Word32)
import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.ByteArray (zero)
import           Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)

type Difficulty = Integer
type Height = Integer
type Timestamp = Word32

data BlockHeader = BlockHeader
    { blockPreviousHash :: Digest SHA256
    , blockRootHash     :: ByteString -- TODO: Should be Digest.
    , blockNonce        :: Word32
    , blockDifficulty   :: Difficulty
    , blockTimestamp    :: Timestamp
    } deriving (Show, Generic)

instance Eq BlockHeader where
    (==) _h _h' = undefined

emptyBlockHeader :: BlockHeader
emptyBlockHeader = BlockHeader
    { blockPreviousHash = zeroHash
    , blockRootHash = zero 32
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

adjustedDifficulty :: Height -> Difficulty -> Difficulty
adjustedDifficulty height startDifficulty =
    startDifficulty `div` height

minDifficulty :: Difficulty
minDifficulty =
    0x0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

genesisDifficulty :: Difficulty
genesisDifficulty =
    0x00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

genesisBlock :: (Binary tx, Foldable t) => Timestamp -> t tx -> Block tx
genesisBlock timestamp xs =
    Block
        BlockHeader
            { blockPreviousHash = zeroHash
            , blockDifficulty   = genesisDifficulty
            , blockTimestamp    = timestamp
            , blockRootHash     = hashTxs xs
            , blockNonce        = 0
            }
        (Seq.fromList (toList xs))

isGenesisBlock :: Block a -> Bool
isGenesisBlock blk =
    (blockPreviousHash . blockHeader) blk == zeroHash

hashTxs :: (Foldable t, Binary tx) => t tx -> ByteString
hashTxs txs
  | Prelude.null txs = zero 32
  | otherwise =
        Merkle.mtHash
      . Merkle.mkMerkleTree
      $ map (toStrict . encode) (toList txs)

hashTx :: Binary tx => tx -> Digest SHA256
hashTx tx =
    hashlazy (encode tx)
