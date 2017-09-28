module Crypto.Blockchain where

import           Crypto.Blockchain.Log
import           Crypto.Blockchain.Block
import           Crypto.Blockchain.Types
import           Crypto.Blockchain.Message (Message)
import qualified Crypto.Blockchain.Message as Message
import           Crypto.Blockchain.Mempool

import           Crypto.Hash (Digest, SHA256(..), hashlazy, digestFromByteString)
import qualified Crypto.Hash.MerkleTree as Merkle

import           Data.Maybe (fromJust)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Binary (Binary, encode)
import           Data.ByteString.Lazy (toStrict)
import           Data.Foldable (toList)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty((:|)), (<|))
import           Data.Time.Clock.POSIX
import           Control.Monad.Reader
import           Control.Monad.Logger
import           Control.Monad.IO.Class (MonadIO)
import           Control.Concurrent.STM
import           Control.Applicative (Alternative)

type Blockchain tx = NonEmpty (Block tx)
type MonadEnv tx m = MonadReader (Env tx) m

class Alternative m => MonadBlock tx m where
    readBlockchain   :: MonadEnv tx m => m (Blockchain tx)
    proposeBlock     :: MonadEnv tx m => Block tx -> m ()
    updateBlockchain :: MonadEnv tx m => Block tx -> m ()

instance MonadBlock tx STM where
    readBlockchain =
        asks envBlockchain >>= readTVar
    updateBlockchain blk = do
        blks <- asks envBlockchain
        modifyTVar blks (\blks -> blk <| blks)

    proposeBlock _ = undefined

class Ord tx => MonadMempool tx m where
    readMempool   :: MonadEnv tx m => m (Set tx)
    writeMempool  :: MonadEnv tx m => Foldable t => t tx -> m ()
    reapMempool   :: MonadEnv tx m => Foldable t => t tx -> m ()

instance Ord tx => MonadMempool tx STM where
    readMempool = do
        mp <- asks envMempool
        fromMempool <$> readTVar mp
    writeMempool txs = do
        mp <- asks envMempool
        modifyTVar mp (addTxs txs)
    reapMempool txs = do
        mp <- asks envMempool
        modifyTVar mp (removeTxs txs)

class Monad m => MonadTime m where
    getTime :: m Timestamp

instance MonadTime IO where
    getTime = round <$> getPOSIXTime

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

proofOfWork :: Monad m => (BlockHeader -> Bool) -> BlockHeader -> m BlockHeader
proofOfWork validate bh | validate bh =
    pure bh
proofOfWork validate bh@BlockHeader { blockNonce } =
    proofOfWork validate bh { blockNonce = blockNonce + 1 }

hasProofOfWork :: BlockHeader -> Bool
hasProofOfWork header = True

findBlock
    :: (Binary tx, MonadTime m, Foldable t)
    => BlockHeader
    -> t tx
    -> m (Maybe (Block tx))
findBlock prevHeader txs = do
    now <- getTime
    proofHeader <- proofOfWork hasProofOfWork (newHeader now)
    pure Nothing
  where
    newHeader t = BlockHeader
        { blockPreviousHash = blockHeaderHash prevHeader
        , blockRootHash     = txsHash
        , blockDifficulty   = undefined
        , blockTimestamp    = t
        , blockNonce        = 0
        }
    txsHash =
        if   null txs
        then zeroHash
        else fromJust . digestFromByteString
                      . Merkle.mtHash
                      . Merkle.mkMerkleTree
                      $ map (toStrict . encode) (toList txs)

blockHash :: Binary a => Block a -> Digest SHA256
blockHash blk = blockHeaderHash (blockHeader blk)

blockHeaderHash :: BlockHeader -> Digest SHA256
blockHeaderHash header =
    hashlazy $ encode header

lastBlock :: Blockchain tx -> Block tx
lastBlock = NonEmpty.head

data Env tx = Env
    { envBlockchain :: TVar (Blockchain tx)
    , envMempool    :: TVar (Mempool tx)
    , envLogger     :: Logger
    , envSeen       :: Set (Hashed (Message tx) SHA256)
    }

newEnv :: Ord tx => Block tx -> STM (Env tx)
newEnv genesis = do
    bc <- newTVar (genesis :| [])
    mp <- newTVar mempty
    pure $ Env
        { envBlockchain = bc
        , envMempool    = mp
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
       , MonadLogger m
       , MonadTime m
       , Binary tx )
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
    reapMempool (blockData blk)
    updateBlockchain blk
processMessage Message.Ping =
    pure ()

-- | Sync the blockchain to the current height.
syncToCurrentHeight :: MonadIO m => m ()
syncToCurrentHeight = undefined
