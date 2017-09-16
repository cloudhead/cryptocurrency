module Bitcoin where

import           Bitcoin.Network
import           Bitcoin.Crypto
import           Bitcoin.Tx
import           Bitcoin.Types

import           Crypto.Blockchain
import           Crypto.Blockchain.Block
import           Crypto.Blockchain.Log
import qualified Crypto.Blockchain.Message as Message
import           Crypto.Blockchain.Message (Message)

import           Crypto.Hash (Digest, SHA256(..), HashAlgorithm, hashlazy, digestFromByteString, hashDigestSize)
import           Crypto.Hash.Tree (HashTree)
import qualified Crypto.Hash.Tree as HashTree
import           Crypto.Error (CryptoFailable(CryptoPassed))
import           Crypto.Number.Serialize (os2ip)

import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, readTVar, modifyTVar)
import           Control.Concurrent.Async (async, race)
import           Control.Concurrent (threadDelay)
import           Control.Monad (forever)
import           Control.Monad.Reader
import           Control.Monad.Logger

import           Data.Binary (Binary, get, put, encode)
import           Data.ByteString hiding (putStrLn)
import           Data.ByteString.Lazy (toStrict)
import           Data.ByteString.Base58
import           Data.ByteArray (convert, ByteArray, zero)
import qualified Data.ByteArray as ByteArray
import           Data.Foldable (toList)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import           Data.Sequence   (Seq, (|>))
import           Data.IORef
import           Data.Word (Word64, Word32)
import           Data.Int (Int32)

import qualified Network.Socket as NS
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           Debug.Trace

newtype Valid a = Valid a

type Bitcoin = Blockchain Tx'

class Validate a where
    validate :: a -> Either Error a

instance Validate (Block a) where
    validate = validateBlock

instance Validate (Blockchain a) where
    validate = validateBlockchain

validateBlockchain :: Blockchain a -> Either Error (Blockchain a)
validateBlockchain bc = Right bc

validateBlock :: Block a -> Either Error (Block a)
validateBlock blk = Right blk

isGenesisBlock :: Block' a -> Bool
isGenesisBlock blk =
    (blockPreviousHash . blockHeader) blk == zeroHash

maxHash :: HashAlgorithm a => Digest a
maxHash = fromJust $
    digestFromByteString (ByteArray.replicate (hashDigestSize SHA256) maxBound :: ByteString)

io :: MonadIO m => IO a -> m a
io = liftIO

startNode
    :: (MonadReader (Env Tx') m, MonadLogger m, MonadIO m)
    => NS.ServiceName
    -> [(NS.HostName, NS.ServiceName)]
    -> m ()
startNode port peers = do
    net :: Internet (Message Tx') <- listen port
    io . async $ connectToPeers net peers
    io . async $ forever $ do
        broadcast net Message.Ping
        threadDelay $ 1000 * 1000

    forever $ do
        msg <- receive net
        case msg of
            Message.Tx tx -> do
                logInfoN "Tx"
                mp <- asks envMempool
                io . atomically $ modifyTVar mp (addTx tx)
            Message.Block blk ->
                logInfoN "Block"
            Message.Ping ->
                logInfoN "Ping"

broadcastTransaction :: Socket n (Message Tx') => n -> Tx' -> IO ()
broadcastTransaction net tx = do
    broadcast net (Message.Tx tx)

block :: [a] -> Either Error (Block' a)
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

genesisBlock :: [a] -> Either Error (Block' a)
genesisBlock xs = validate $
    Block
        BlockHeader
            { blockPreviousHash = zeroHash
            , blockDifficulty   = undefined
            , blockTimestamp    = undefined
            , blockRootHash     = undefined
            , blockNonce        = undefined
            }
        (Seq.fromList xs)

blockchain :: [Block' a] -> Either Error (Blockchain a)
blockchain blks = validate $ Seq.fromList blks

hashValidation :: Integer -> BlockHeader -> Bool
hashValidation target bh =
    digest > zeroHash
  where
    digest = hashlazy $ encode bh :: Digest SHA256

proofOfWork :: (BlockHeader -> Bool) -> BlockHeader -> BlockHeader
proofOfWork validate bh | validate bh =
    bh
proofOfWork validate bh@BlockHeader { blockNonce } =
    proofOfWork validate bh { blockNonce = blockNonce + 1 }

appendBlock :: Binary a => Seq a -> Blockchain a -> Either Error (Blockchain a)
appendBlock dat bc =
    validate $ bc |> new
  where
    prev = Seq.index bc (Seq.length bc - 1)
    new = Block header dat
    header = BlockHeader
        { blockPreviousHash = blockHash prev
        , blockRootHash     = rootHash
        , blockDifficulty   = undefined
        , blockTimestamp    = undefined
        , blockNonce        = undefined
        }
    rootHash =
        HashTree.rootHash . HashTree.fromList . NonEmpty.fromList . toList $
            fmap (hashlazy . encode) dat

blockHash :: (Binary a) => Block a -> Digest SHA256
blockHash blk =
    hashlazy $ encode blk

connectToPeers :: Internet a -> [(NS.HostName, NS.ServiceName)] -> IO ()
connectToPeers net peers =
    mapM_ (connect net) peers

