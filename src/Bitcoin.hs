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

processMessage
    :: ( MonadLogger m
       , MonadIO m
       , MonadSTM m
       , MonadConc m )
    => Env tx m
    -> Message tx
    -> m ()
processMessage Env { envMempool } (Message.Tx tx) = do
    logInfoN "Tx"
    modifyTVar envMempool (addTx tx)
processMessage Env { envNewBlocks } (Message.Block blk) = do
    logInfoN "Block"
    putTMVar envNewBlocks blk
processMessage _ Message.Ping =
    logInfoN "Ping"

startNode
    :: ( MonadReader (Env Tx' m) m
       , MonadLogger m
       , MonadIO m
       , MonadSTM m
       , MonadConc m )
    => NS.ServiceName
    -> [(NS.HostName, NS.ServiceName)]
    -> m ()
startNode port peers = do
    net :: Internet (Message Tx') <- listen port
    io . async $ connectToPeers net peers
    io . async $ forever $ do
        broadcast net Message.Ping
        threadDelay $ 1000 * 1000

    env@Env {..} <- ask
    forever $ do
        msg <- receive net
        when (not (messageAlreadySeen msg envSeen)) $ do
            processMessage env msg
            broadcast net msg

broadcastTransaction :: Socket n (Message Tx') => n -> Tx' -> IO ()
broadcastTransaction net tx = do
    broadcast net (Message.Tx tx)

connectToPeers :: Internet a -> [(NS.HostName, NS.ServiceName)] -> IO ()
connectToPeers net peers =
    mapM_ (connect net) peers

