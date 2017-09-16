module Crypto.Blockchain.Block where

import           Crypto.Blockchain.Hash

import           Data.Binary (Binary, get, put, encode)
import qualified Data.Sequence as Seq
import           Data.Sequence   (Seq, (|>), (><))
import           Crypto.Hash (Digest, SHA256(..), HashAlgorithm, hashlazy, digestFromByteString, hashDigestSize)
import           Crypto.Hash.Tree (HashTree)
import qualified Crypto.Hash.Tree as HashTree
import           Crypto.Error (CryptoFailable(CryptoPassed))
import           Crypto.Number.Serialize (os2ip)
import           Data.Word (Word64, Word32)
import           Data.ByteString hiding (putStrLn)
import           Data.ByteString.Lazy (toStrict)
import           Data.ByteString.Base58
import           Data.ByteArray (convert, ByteArray, zero)
import           Data.Maybe (fromJust)
import           Data.Foldable (toList)
import           Control.Monad (forever)
import           Control.Monad.Reader
import           Control.Monad.Logger
import           Control.Monad.STM.Class (MonadSTM, liftSTM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Concurrent.STM (TVar, readTVar, modifyTVar, newTVarIO, atomically)
import           Control.Concurrent.Async (async, race)
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
    (==) h h' = undefined

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

