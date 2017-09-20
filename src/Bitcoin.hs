module Bitcoin where

import           Bitcoin.Network
import           Bitcoin.Tx

import           Crypto.Blockchain
import qualified Crypto.Blockchain.Message as Message
import           Crypto.Blockchain.Message (Message)

import           Control.Concurrent.Async (async, concurrently_)
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
       , MonadMempool tx m
       , MonadBlock tx m
       , MonadIO m )
    => Env tx
    -> Message tx
    -> m ()
processMessage env (Message.Tx tx) = do
    logInfoN "Tx"
    writeMempool env [tx]
processMessage env (Message.Block blk) = do
    logInfoN "Block"
    receiveNewBlock env blk
processMessage _ Message.Ping =
    logInfoN "Ping"

startNode
    :: ( MonadReader (Env Tx') m
       , MonadMempool Tx' m
       , MonadBlock Tx' m
       , MonadLogger m
       , IO ~ m )
    => NS.ServiceName
    -> [(NS.HostName, NS.ServiceName)]
    -> m ()
startNode port peers = do
    net :: Internet (Message Tx') <- listen port
    async $ connectToPeers net peers
    async $ forever $ do
        broadcast net Message.Ping
        threadDelay $ 1000 * 1000

    concurrently_ (forever mineBlock)
                  (forever (listenForMessage net))

listenForMessage
    :: ( MonadReader (Env Tx') m
       , MonadLogger m
       , MonadMempool Tx' m
       , MonadBlock Tx' m
       , MonadIO m
       , Socket net (Message Tx')
       ) => net -> m ()
listenForMessage net = do
    env@Env {..} <- ask
    forever $ do
        msg <- receive net
        when (messageIsNew msg envSeen) $ do
            processMessage env msg
            broadcast net msg

broadcastTransaction :: Socket n (Message Tx') => n -> Tx' -> IO ()
broadcastTransaction net tx = do
    broadcast net (Message.Tx tx)

connectToPeers :: Internet a -> [(NS.HostName, NS.ServiceName)] -> IO ()
connectToPeers net peers =
    mapM_ (connect net) peers

