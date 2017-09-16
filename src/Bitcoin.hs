module Bitcoin where

import           Bitcoin.Network
import           Bitcoin.Tx

import           Crypto.Blockchain
import qualified Crypto.Blockchain.Message as Message
import           Crypto.Blockchain.Message (Message)

import           Control.Concurrent.Classy (MonadConc)
import           Control.Concurrent.Classy.Async (async)
import           Control.Concurrent.Classy.STM
import           Control.Concurrent (threadDelay)
import           Control.Monad (forever)
import           Control.Monad.Reader
import           Control.Monad.Logger

import qualified Network.Socket as NS

newtype Valid a = Valid a

type Bitcoin = Blockchain Tx'

io :: MonadIO m => IO a -> m a
io = liftIO

startNode
    :: (MonadReader (Env Tx' m) m, MonadLogger m, MonadIO m, MonadSTM m, MonadConc m)
    => NS.ServiceName
    -> [(NS.HostName, NS.ServiceName)]
    -> m ()
startNode port peers = do
    net :: Internet (Message Tx') <- listen port
    io . async $ connectToPeers net peers
    io . async $ forever $ do
        broadcast net Message.Ping
        threadDelay $ 1000 * 1000

    Env {..} <- ask

    forever $ do
        msg <- receive net
        case msg of
            Message.Tx tx -> do
                logInfoN "Tx"
                modifyTVar envMempool (addTx tx)
            Message.Block blk -> do
                logInfoN "Block"
                putTMVar envNewBlocks blk
            Message.Ping ->
                logInfoN "Ping"

broadcastTransaction :: Socket n (Message Tx') => n -> Tx' -> IO ()
broadcastTransaction net tx = do
    broadcast net (Message.Tx tx)

connectToPeers :: Internet a -> [(NS.HostName, NS.ServiceName)] -> IO ()
connectToPeers net peers =
    mapM_ (connect net) peers

