{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Bitcoin.Network
    ( Socket(..)
    , Node(..)
    , Internet
    , TestNetwork
    , listen
    , createTestNetwork
    ) where

import           Bitcoin.Crypto

import qualified Network.Socket.ByteString as NSB
import qualified Network.Socket as NS
import qualified Network.Simple.TCP as Net
import           Data.Binary (Binary)
import qualified Data.Binary as Binary (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B hiding (pack)
import           Data.IORef
import           Control.Concurrent.STM (TChan, writeTQueue, readTQueue, atomically, dupTChan)
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.Async (concurrently, async)
import           Control.Monad
import           GHC.Generics
import           Pipes

class Socket s a | s -> a where
    type Remote s

    connect   :: MonadIO m => s -> Remote s -> m ()
    broadcast :: MonadIO m => s -> a -> m ()
    receive   :: MonadIO m => s -> m a

data Internet a = Internet
    { internetIncoming :: TChan a
    , internetOutgoing :: TChan a
    , internetRemotes  :: IORef [(Net.Socket, Net.SockAddr)]
    }

instance Socket (Internet a) a where
    type Remote (Internet a) = (NS.HostName, NS.ServiceName)

    connect (Internet _ _ remotes) (host, port) = liftIO $ do
        remote <- Net.connectSock host port
        modifyIORef remotes (remote :)
    broadcast (Internet _ out _) msg =
        liftIO $ atomically $ writeTChan out msg
    receive (Internet inc _ _) =
        liftIO $ atomically $ readTChan inc

type TestNetwork a = [Node (TestSocket a)]

data TestSocket a = TestSocket
    { testSocketChan    :: TChan a
    , testSocketRemotes :: IORef [(Net.HostName, TestSocket a)]
    }

data Node s = Node
    { nodeName    :: NS.HostName
    , nodeSocket  :: s
    , nodeKeyPair :: (PublicKey, PrivateKey)
    }

instance Socket (TestSocket a) a where
    type Remote (TestSocket a) = (Net.HostName, TestSocket a)

    connect (TestSocket _ remotes) remote =
        liftIO $ modifyIORef remotes (remote :)
    broadcast (TestSocket _ remotes') msg = liftIO $ do
        remotes <- readIORef remotes'
        forM_ remotes $ \(_, sock) ->
            atomically $ writeTChan (testSocketChan sock) msg
    receive (TestSocket inc _) =
        liftIO $ atomically $ readTChan inc

listenTest :: (MonadIO m, Binary a) => NS.ServiceName -> m (TestSocket a)
listenTest svc = liftIO $ do
    remotes  <- newIORef []
    incoming <- newTChanIO
    pure $ TestSocket incoming remotes

createTestNetwork :: (MonadIO m, Binary a) => [NS.ServiceName] -> m (TestNetwork a)
createTestNetwork svcs = liftIO $ do
    nodes :: [Node (TestSocket a)] <- forM svcs $ \svc -> do
        keys <- generateKeyPair
        sock <- listenTest svc
        pure $ Node svc sock keys
    forM_ nodes $ \(Node svc sock _) ->
        mapM_ (connect sock) $ map (\(Node svc sock _) -> (svc, sock))
                             $ filter (\(Node svc' _ _) -> svc' /= svc) nodes
    pure nodes

packetSize :: Int
packetSize = 4096

-- | Connect to the internet and listen for messages.
listen
    :: (MonadIO m, Binary a)
    => NS.ServiceName
    -> m (Internet a)
listen port = liftIO $ do
    remotes  <- newIORef []
    incoming <- newTChanIO
    outgoing <- newBroadcastTChanIO
    async $ Net.serve Net.HostAny port $ \(sock, addr) -> do
        outgoing' <- atomically $ dupTChan outgoing

        void $ concurrently
            (runEffect $ fromSocket sock packetSize >-> pipeIncomming >-> toQueue incoming)
            (runEffect $ fromQueue outgoing'        >-> pipeOutgoing  >-> toRemotes remotes)

    pure $ Internet incoming outgoing remotes
  where
    hints = NS.defaultHints { NS.addrFlags = [NS.AI_PASSIVE] }

pipeIncomming :: (MonadIO m, Binary a) => Pipe ByteString a m ()
pipeIncomming = forever $ do
    bytes <- await
    yield (Binary.decode $ LBS.fromStrict bytes)

pipeOutgoing :: (MonadIO m, Binary a) => Pipe a ByteString m ()
pipeOutgoing = forever $ do
    out <- await
    yield (LBS.toStrict $ Binary.encode out)

fromSocket :: MonadIO m => NS.Socket -> Int -> Producer' ByteString m ()
fromSocket sock nbytes = forever $ do
    msg@(bs, _) <- liftIO (NSB.recvFrom sock nbytes)
    unless (B.null bs) $ yield bs
{-# INLINABLE fromSocket #-}

toRemotes :: MonadIO m => IORef [(NS.Socket, NS.SockAddr)] -> Consumer' ByteString m r
toRemotes remotes' = for cat $ \b -> do
    remotes <- liftIO $ readIORef remotes'
    mapM_ (\(sock, _) -> liftIO $ NSB.sendAll sock b) remotes
{-# INLINABLE toRemotes #-}

fromQueue :: MonadIO m => TChan c -> Producer' c m ()
fromQueue queue = forever $
    yield =<< liftIO (atomically $ readTChan queue)
{-# INLINABLE fromQueue #-}

toQueue :: MonadIO m => TChan c -> Consumer' c m r
toQueue queue = for cat (liftIO . atomically . writeTChan queue)
{-# INLINABLE toQueue #-}
