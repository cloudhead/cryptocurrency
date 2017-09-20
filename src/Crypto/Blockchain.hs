module Crypto.Blockchain where

import           Crypto.Blockchain.Log
import           Crypto.Blockchain.Block
import           Crypto.Blockchain.Types
import           Crypto.Blockchain.Message (Message)

import           Crypto.Hash (Digest, SHA256(..), hashlazy)
import qualified Crypto.Hash.Tree as HashTree

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Binary (Binary, encode)
import qualified Data.Sequence as Seq
import           Data.Sequence   (Seq, (|>), (><))
import           Data.Foldable (toList)
import qualified Data.List.NonEmpty as NonEmpty
import           Control.Monad.Reader
import           Control.Monad.Logger
import           Control.Monad.IO.Class (MonadIO)
import           Control.Concurrent.STM
import           Control.Applicative (Alternative, (<|>))

type Blockchain tx = Seq (Block tx)

class Alternative m => MonadBlock tx m where
    listenForBlock  :: Env tx -> m (Block tx)
    receiveNewBlock :: Env tx -> Block tx -> m ()
    proposeBlock    :: Env tx -> Block tx -> m ()
    findBlock       :: Traversable t => Env tx -> t tx -> m (Block tx)

instance MonadBlock tx STM where
    listenForBlock Env { envNewBlocks } =
        takeTMVar envNewBlocks
    receiveNewBlock Env { envNewBlocks } blk =
        putTMVar envNewBlocks blk
    proposeBlock _ _ = undefined
    findBlock _ _ = undefined

class MonadMempool tx m where
    readMempool   :: Env tx -> m (Seq tx)
    writeMempool  :: Traversable t => Env tx -> t tx -> m ()
    updateMempool :: Traversable t => Env tx -> t tx -> m ()

instance MonadMempool tx STM where
    readMempool Env { envMempool } =
        fromMempool <$> readTVar envMempool
    writeMempool Env { envMempool } txs =
        modifyTVar envMempool (addTxs txs)
    updateMempool Env { envMempool } txs =
        undefined

instance Validate (Blockchain a) where
    validate = validateBlockchain

validateBlockchain :: Blockchain a -> Either Error (Blockchain a)
validateBlockchain bc = Right bc

blockchain :: [Block a] -> Either Error (Blockchain a)
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

newtype Mempool tx = Mempool { fromMempool :: Seq tx }
    deriving (Show, Monoid)

addTx :: tx -> Mempool tx -> Mempool tx
addTx tx (Mempool txs) = Mempool (txs |> tx)

addTxs :: Foldable t => t tx -> Mempool tx -> Mempool tx
addTxs txs' (Mempool txs) =
    Mempool $ txs >< Seq.fromList (toList txs')

data Env tx = Env
    { envBlockchain :: TVar (Blockchain tx)
    , envMempool    :: TVar (Mempool tx)
    , envNewBlocks  :: TMVar (Block tx)
    , envLogger     :: Logger
    , envSeen       :: Set (Hashed (Message tx) SHA256)
    }

newEnv :: STM (Env tx)
newEnv = do
    bc <- newTVar mempty
    mp <- newTVar mempty
    nb <- newEmptyTMVar
    pure $ Env
        { envBlockchain = bc
        , envMempool    = mp
        , envNewBlocks  = nb
        , envLogger     = undefined
        , envSeen       = mempty
        }

messageIsNew :: Binary tx => Message tx -> Set (Hashed (Message tx) SHA256) -> Bool
messageIsNew msg = not . Set.member (hashed msg)

mineBlock
    :: ( MonadIO m
       , MonadMempool tx m
       , MonadBlock tx m
       , MonadReader (Env tx) m
       , MonadLogger m)
    => m ()
mineBlock = do
    env <- ask
    txs <- readMempool env
    result <- Left <$> findBlock env txs <|> Right <$> listenForBlock env
    case result of
        Left foundBlock ->
            proposeBlock env foundBlock
        Right receivedBlock ->
            updateMempool env (blockData receivedBlock)
