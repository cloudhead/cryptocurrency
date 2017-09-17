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
import           Control.Monad (forever)
import           Control.Monad.Reader
import           Control.Monad.Logger
import           Control.Monad.STM.Class (MonadSTM)
import           Control.Monad.Conc.Class (MonadConc)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Concurrent.Classy.STM

type Blockchain tx = Seq (Block tx)

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

data Env tx stm = Env
    { envBlockchain :: TVar stm (Blockchain tx)
    , envMempool    :: TVar stm (Mempool tx)
    , envNewBlocks  :: TMVar stm (Block tx)
    , envLogger     :: Logger
    , envSeen       :: Set (Hashed (Message tx) SHA256)
    }

newEnv :: MonadSTM stm => stm (Env tx stm)
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

messageAlreadySeen :: Binary tx => Message tx -> Set (Hashed (Message tx) SHA256) -> Bool
messageAlreadySeen msg = Set.member (hashed msg)

findBlock :: (MonadSTM m, Traversable t) => Env tx m -> t tx -> m (Block tx)
findBlock = undefined

listenForBlock :: MonadSTM m => Env tx m -> m (Block tx)
listenForBlock Env { envNewBlocks } =
    takeTMVar envNewBlocks

proposeBlock :: MonadIO m => Block tx -> m ()
proposeBlock = undefined

updateMempool :: (MonadSTM m, Traversable t) => Env tx m -> t tx -> m ()
updateMempool Env { envMempool } txs =
    modifyTVar envMempool (addTxs txs)

readTransactions :: MonadSTM m => Env tx m -> m (Seq tx)
readTransactions Env { envMempool } =
    fromMempool <$> readTVar envMempool

mineBlocks :: (MonadSTM m, MonadConc m, MonadIO m, MonadReader (Env tx m) m, MonadLogger m) => m ()
mineBlocks = forever $ do
    env <- ask
    txs <- readTransactions env
    result <- (Left <$> findBlock env txs) `orElse` (Right <$> listenForBlock env)
    case result of
        Left foundBlock ->
            proposeBlock foundBlock
        Right receivedBlock ->
            updateMempool env (blockData receivedBlock)
