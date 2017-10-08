{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Crypto.Blockchain where

import           Crypto.Blockchain.Log
import           Crypto.Blockchain.Block
import           Crypto.Blockchain.Types
import           Crypto.Blockchain.Message (Message)
import qualified Crypto.Blockchain.Message as Message
import           Crypto.Blockchain.Mempool

import           Crypto.Hash (Digest, SHA256(..), hashlazy)

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Foldable (toList)
import           Data.Binary (Binary, encode, Word32)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as Base16
import           Data.ByteArray (ByteArrayAccess, convert)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty((:|)), (<|))
import           Data.Time.Clock.POSIX
import           Control.Monad.Reader
import           Control.Monad.Logger
import           Control.Monad.IO.Class (MonadIO)
import           Control.Concurrent.STM
import           Control.Applicative (Alternative)
import           Text.Printf
import           GHC.Records

type Blockchain tx = NonEmpty (Block tx)

type MonadEnv tx m = MonadReader (Env tx) m
type HasBlocks r tx = HasField "envBlockchain" r (TVar (Blockchain tx))
type HasHeight r = HasField "envHeight" r (TVar Height)

class Alternative m => MonadBlock tx m where
    readBlockchain   :: (HasBlocks r tx, MonadReader r m) => m (Blockchain tx)
    proposeBlock     :: (HasBlocks r tx, MonadReader r m) => Block tx -> m ()
    updateBlockchain :: (HasBlocks r tx, MonadReader r m) => Block tx -> m ()
    readHeight       :: (HasHeight r, MonadReader r m, tx ~ ()) => m Height

instance MonadBlock tx STM where
    readBlockchain =
        asks (getField @"envBlockchain") >>= readTVar
    updateBlockchain blk = do
        blks <- asks (getField @"envBlockchain")
        modifyTVar blks (\blks -> blk <| blks)

    proposeBlock _ = undefined
    readHeight =
        asks (getField @"envHeight") >>= readTVar

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

calculateDifficulty :: Blockchain tx -> Difficulty
calculateDifficulty blks =
    if   length range < blocksConsidered
    then genesisDifficulty
    else currentDifficulty * targetElapsed `div` toInteger actualElapsed
  where
    range             = NonEmpty.take blocksConsidered blks
    rangeStart        = blockHeader $ NonEmpty.last (NonEmpty.head blks :| tail range)
    rangeEnd          = blockHeader $ NonEmpty.head blks
    actualElapsed     = blockTimestamp rangeEnd - blockTimestamp rangeStart
    targetElapsed     = fromIntegral $ blocksConsidered * blockTimeSeconds
    blocksConsidered  = timePeriodMinutes `div` blockTimeMinutes
    timePeriodMinutes = timePeriodDays * 24 * 60
    timePeriodDays    = 2 * 7 -- Two weeks.
    blockTimeMinutes  = 10
    blockTimeSeconds  = blockTimeMinutes * 60
    currentDifficulty = blockDifficulty rangeEnd
    genesisDifficulty = blockDifficulty . blockHeader $ NonEmpty.last blks

hasPoW :: BlockHeader -> Bool
hasPoW header =
    difficulty header < blockDifficulty header

findPoW :: Monad m => BlockHeader -> m BlockHeader
findPoW bh@BlockHeader { blockNonce }
    | hasPoW bh =
        pure bh
    | blockNonce <= (maxBound :: Word32) =
        findPoW bh { blockNonce = blockNonce + 1 }
    | otherwise =
        findPoW bh { blockNonce = 0 }

findBlock
    :: (Binary tx, MonadTime m, Foldable t)
    => BlockHeader
    -> Difficulty
    -> t tx
    -> m (Maybe (Block tx))
findBlock prevHeader target txs = do
    now <- getTime
    proofHeader <- findPoW (newHeader now)
    pure Nothing
  where
    newHeader t = BlockHeader
        { blockPreviousHash = blockHeaderHash prevHeader
        , blockRootHash     = hashTxs txs
        , blockDifficulty   = target
        , blockTimestamp    = t
        , blockNonce        = 0
        }

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
    , envHeight     :: TVar Height
    , envLogger     :: Logger
    , envSeen       :: Set (Hashed (Message tx) SHA256)
    }

newEnv :: Ord tx => Block tx -> STM (Env tx)
newEnv genesis = do
    bc <- newTVar (genesis :| [])
    mp <- newTVar mempty
    eh <- newTVar 0
    pure $ Env
        { envBlockchain = bc
        , envHeight     = eh
        , envMempool    = mp
        , envLogger     = undefined
        , envSeen       = mempty
        }

messageIsNew :: Binary tx => Message tx -> Set (Hashed (Message tx) SHA256) -> Bool
messageIsNew msg = not . Set.member (hashed msg)

mineBlock
    :: ( MonadMempool tx m
       , MonadBlock tx m
       , MonadEnv tx m
       , MonadLogger m
       , MonadTime m
       , Binary tx )
    => m ()
mineBlock = do
    txs <- readMempool
    blks <- readBlockchain
    result <- findBlock (blockHeader (lastBlock blks))
                        (calculateDifficulty blks) txs
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
       , MonadEnv tx m )
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

toHex :: ByteArrayAccess ba => ba -> String
toHex bs =
    BS.unpack $ Base16.encode $ convert bs

printBlockchain :: Binary tx => Blockchain tx -> IO ()
printBlockchain (NonEmpty.toList -> blks) = do
    printf "\n"
    forM_ (zip heights blks) $ \(h, Block bh@BlockHeader{..} txs) -> do
        printf "┍━━━ %d ━━━ %s ━━━┑\n" (h :: Int) (toHex $ blockHeaderHash bh)
        printf "│ prevHash:   %-64s │\n" (toHex blockPreviousHash)
        printf "│ timestamp:  %-64d │\n" blockTimestamp
        printf "│ rootHash:   %-64s │\n" (BS.unpack blockRootHash)
        printf "│ difficulty: %064x │\n" (blockDifficulty)
        printf "│ nonce:      %-64d │\n" blockNonce
        printf "├────────%s─────────┤\n" (replicate 61 '─')

        forM_ (zip [0..length txs] (toList txs)) $ \(n, tx) ->
            printf "│ %03d:  %-64s       │\n" n (toHex $ hashTx tx)

        printf "└────────%s─────────┘\n" (replicate 61 '─')
  where
    heights = reverse [0..length blks - 1]
