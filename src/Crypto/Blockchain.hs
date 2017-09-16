module Crypto.Blockchain where

import           Crypto.Blockchain.Log
import           Crypto.Blockchain.Block

import qualified Data.Sequence as Seq
import           Data.Sequence   (Seq, (|>), (><))
import           Data.Foldable (toList)
import           Control.Monad (forever)
import           Control.Monad.Reader
import           Control.Monad.Logger
import           Control.Monad.STM.Class (MonadSTM, liftSTM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Concurrent.STM (TVar, readTVar, modifyTVar, newTVarIO)
import           Control.Concurrent.Async (race)

type Blockchain tx = Seq (Block' tx)

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
    , envLogger     :: Logger
    }

newEnv :: IO (Env tx)
newEnv = do
    bc <- newTVarIO mempty
    mp <- newTVarIO mempty
    pure $ Env
        { envBlockchain = bc
        , envMempool    = mp
        , envLogger     = undefined
        }

findBlock :: (Monad m, Traversable t) => t tx -> m (Block tx)
findBlock = undefined

listenForBlock :: MonadIO m => m (Block tx)
listenForBlock = undefined

proposeBlock :: MonadIO m => Block tx -> m ()
proposeBlock = undefined

updateMempool :: (MonadSTM m, MonadReader (Env tx) m, Traversable t) => t tx -> m ()
updateMempool txs = do
    mp <- asks envMempool
    liftSTM $ modifyTVar mp (addTxs txs)

readTransactions :: (MonadSTM m, MonadReader (Env tx) m) => m (Seq tx)
readTransactions = do
    mp <- asks envMempool
    fromMempool <$> liftSTM (readTVar mp)

mineBlocks :: (MonadSTM m, MonadReader (Env tx) m, MonadLogger m, MonadIO m) => m ()
mineBlocks = forever $ do
    txs <- readTransactions
    result <- liftIO $ race (findBlock txs) (listenForBlock)
    case result of
        Left foundBlock ->
            proposeBlock foundBlock
        Right receivedBlock ->
            updateMempool (blockData receivedBlock)

