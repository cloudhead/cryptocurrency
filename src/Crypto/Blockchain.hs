module Crypto.Blockchain where

import           Crypto.Blockchain.Log
import           Crypto.Blockchain.Block

import qualified Data.Sequence as Seq
import           Data.Sequence   (Seq, (|>), (><))
import           Data.Foldable (toList)
import           Control.Monad (forever)
import           Control.Monad.Reader
import           Control.Monad.Logger
import           Control.Monad.STM.Class (MonadSTM)
import           Control.Monad.Conc.Class (MonadConc)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Concurrent.Classy.STM

type Blockchain tx = Seq (Block' tx)

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
        }

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
