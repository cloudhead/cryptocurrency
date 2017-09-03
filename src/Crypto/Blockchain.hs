module Crypto.Blockchain where

import           Crypto.Blockchain.Hash
import           Crypto.Blockchain.Log

import           Data.Binary (Binary, get, put, encode)
import qualified Data.Sequence as Seq
import           Data.Sequence   (Seq, (|>))
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
import           Control.Monad (forever)
import           Control.Monad.Reader
import           Control.Monad.Logger
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Concurrent (MVar, readMVar, modifyMVar_, newMVar, threadDelay)
import           Control.Concurrent.Async (async, race)
import           GHC.Generics (Generic)

type Timestamp = Word32

type Blockchain tx = Seq (Block' tx)
type Block' tx = Block tx

newtype Mempool tx = Mempool { fromMempool :: Seq tx }
    deriving (Show, Monoid)

addTx :: tx -> Mempool tx -> Mempool tx
addTx tx (Mempool txs) = Mempool (txs |> tx)

data Env tx = Env
    { envBlockchain :: MVar (Blockchain tx)
    , envMempool    :: MVar (Mempool tx)
    , envLogger     :: Logger
    }

newEnv :: IO (Env tx)
newEnv = do
    bc <- newMVar mempty
    mp <- newMVar mempty
    pure $ Env
        { envBlockchain = bc
        , envMempool    = mp
        , envLogger     = undefined
        }

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

type Difficulty = Integer

difficulty :: BlockHeader -> Difficulty
difficulty bh = os2ip (hashlazy $ encode bh :: Digest SHA256)

data Block a = Block
    { blockHeader :: BlockHeader
    , blockData   :: Seq a
    } deriving (Show, Generic)

instance (Binary a) => Binary (Block a)
deriving instance Eq a => Eq (Block a)

zeroHash :: HashAlgorithm a => Digest a
zeroHash = fromJust $
    digestFromByteString (zero (hashDigestSize SHA256) :: ByteString)

findBlock :: (Monad m, Traversable t) => t tx -> m (Block tx)
findBlock = undefined

listenForBlock :: MonadIO m => m (Block tx)
listenForBlock = undefined

proposeBlock :: MonadIO m => Block tx -> m ()
proposeBlock = undefined

updateMempool :: (MonadReader (Env tx) m, Traversable t) => t tx -> m ()
updateMempool = undefined

readTransactions :: MonadReader (Env tx) m => m (Seq tx)
readTransactions = undefined

mineBlocks :: (MonadReader (Env tx) m, MonadLogger m, MonadIO m) => m ()
mineBlocks = forever $ do
    txs <- readTransactions
    result <- liftIO $ race (findBlock txs) (listenForBlock)
    case result of
        Left foundBlock ->
            proposeBlock foundBlock
        Right receivedBlock ->
            updateMempool (blockData receivedBlock)

