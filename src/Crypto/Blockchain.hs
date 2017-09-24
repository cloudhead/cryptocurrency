module Crypto.Blockchain where

import           Crypto.Blockchain.Log
import           Crypto.Blockchain.Block
import           Crypto.Blockchain.Types
import           Crypto.Blockchain.Message (Message)
import qualified Crypto.Blockchain.Message as Message

import           Crypto.Hash (Digest, SHA256(..), hashlazy)
import qualified Crypto.Hash.Tree as HashTree

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Binary (Binary, encode)
import qualified Data.Sequence as Seq
import           Data.Sequence   (Seq, (|>), (><))
import           Data.Foldable (toList)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty((:|)), (<|))
import           Control.Monad.Reader
import           Control.Monad.Logger
import           Control.Monad.IO.Class (MonadIO)
import           Control.Concurrent.STM
import           Control.Applicative (Alternative)

type Blockchain tx = NonEmpty (Block tx)
type MonadEnv tx m = MonadReader (Env tx) m

class Alternative m => MonadBlock tx m where
    readBlockchain   :: MonadEnv tx m => m (Blockchain tx)
    listenForBlock   :: MonadEnv tx m => m (Block tx)
    proposeBlock     :: MonadEnv tx m => Block tx -> m ()
    updateBlockchain :: MonadEnv tx m => Block tx -> m ()
    findBlock        :: Traversable t => BlockHeader -> t tx -> m (Maybe (Block tx))

instance MonadBlock tx STM where
    readBlockchain =
        asks envBlockchain >>= readTVar
    listenForBlock =
        takeTMVar =<< asks envNewBlocks
    updateBlockchain blk = do
        blks <- asks envBlockchain
        modifyTVar blks (\blks -> blk <| blks)

    proposeBlock _ = undefined
    findBlock _ _ = undefined

class MonadMempool tx m where
    readMempool   :: MonadEnv tx m => m (Seq tx)
    writeMempool  :: MonadEnv tx m => Traversable t => t tx -> m ()
    updateMempool :: MonadEnv tx m => Traversable t => t tx -> m ()

instance MonadMempool tx STM where
    readMempool = do
        mp <- asks envMempool
        fromMempool <$> readTVar mp
    writeMempool txs = do
        mp <- asks envMempool
        modifyTVar mp (addTxs txs)
    updateMempool txs =
        undefined

instance Validate (Blockchain a) where
    validate = validateBlockchain

validateBlockchain :: Blockchain a -> Either Error (Blockchain a)
validateBlockchain bc = Right bc

blockchain :: [Block a] -> Either Error (Blockchain a)
blockchain blks = validate $ NonEmpty.fromList blks

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
    validate $ new <| bc
  where
    prev = NonEmpty.head bc
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

lastBlock :: Blockchain tx -> Block tx
lastBlock = NonEmpty.head

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

newEnv :: Block tx -> STM (Env tx)
newEnv genesis = do
    bc <- newTVar (genesis :| [])
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
       , MonadEnv tx m
       , MonadLogger m)
    => m ()
mineBlock = do
    txs <- readMempool
    blks <- readBlockchain
    result <- findBlock (blockHeader (lastBlock blks)) txs
    case result of
        Just foundBlock -> do
            proposeBlock foundBlock
            updateBlockchain foundBlock
        Nothing ->
            pure ()

processMessage
    :: ( MonadLogger m
       , MonadMempool tx m
       , MonadBlock tx m
       , MonadEnv tx m
       , MonadIO m )
    => Message tx
    -> m ()
processMessage (Message.Tx tx) = do
    writeMempool [tx]
processMessage (Message.Block blk) = do
    -- TODO: Validate, check previous hash, if height <= my chain height, discard,
    -- else try to update my chain. If the block height isnt my chain's height + 1,
    -- get previous blocks from network and add to my chain. This can happen
    -- if I've been building on the wrong history.
    updateMempool (blockData blk)
    updateBlockchain blk
processMessage Message.Ping =
    pure ()
