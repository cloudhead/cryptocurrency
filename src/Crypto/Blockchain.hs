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
import           Control.Concurrent.STM
import           Control.Applicative ((<|>))

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
    , envNewBlocks  :: TMVar (Block tx)
    , envLogger     :: Logger
    }

newEnv :: IO (Env tx)
newEnv = do
    bc <- newTVarIO mempty
    mp <- newTVarIO mempty
    nb <- newEmptyTMVarIO
    pure $ Env
        { envBlockchain = bc
        , envMempool    = mp
        , envNewBlocks  = nb
        , envLogger     = undefined
        }

findBlock :: (MonadSTM m, Traversable t) => Env tx -> t tx -> m (Block tx)
findBlock = undefined

listenForBlock :: Env tx -> STM (Block tx)
listenForBlock Env { envNewBlocks } =
    takeTMVar envNewBlocks

proposeBlock :: MonadIO m => Block tx -> m ()
proposeBlock = undefined

updateMempool :: (MonadSTM m, Traversable t) => Env tx -> t tx -> m ()
updateMempool Env { envMempool } txs =
    liftSTM $ modifyTVar envMempool (addTxs txs)

readTransactions :: Env tx -> STM (Seq tx)
readTransactions Env { envMempool } =
    fromMempool <$> readTVar envMempool

mineBlocks :: (MonadIO m, MonadReader (Env tx) m, MonadLogger m) => m ()
mineBlocks = forever $ do
    env <- ask
    liftIO $ do
        txs    <- atomically $ readTransactions env
        result <- atomically $  Left  <$> findBlock env txs
                            <|> Right <$> listenForBlock env
        case result of
            Left foundBlock ->
                proposeBlock foundBlock
            Right receivedBlock ->
                atomically $ updateMempool env (blockData receivedBlock)
