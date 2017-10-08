{-# LANGUAGE ScopedTypeVariables #-}
module Crypto.Blockchain.Test.Arbitrary where

import           Crypto.Blockchain
import           Crypto.Blockchain.Block
import           Crypto.Hash

import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty((:|)), (<|))
import           Data.Maybe (fromJust)
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import           Data.Binary (Binary)
import           Data.Word (Word8)
import           Control.Monad (replicateM)

import           Test.QuickCheck

instance Arbitrary BS.ByteString where
    arbitrary = do
        str <- arbitrary
        pure $ BS.pack str

instance (Arbitrary tx, Binary tx) => Arbitrary (Block tx) where
    arbitrary =
        Block <$> arbitrary <*> arbitrary

instance (Arbitrary tx, Binary tx) => Arbitrary (Blockchain tx) where
    arbitrary = do
        genesis <- arbitraryGenesis
        rest <- arbitrary :: Gen [Block tx]
        pure $ genesis :| rest

instance Arbitrary (Digest SHA256) where
    arbitrary = do
        str <- replicateM 32 (arbitrary :: Gen Word8)
        pure . fromJust $ digestFromByteString (BS.pack str)

instance Arbitrary BlockHeader where
    arbitrary =
        BlockHeader
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

arbitraryBlock' :: forall tx. (Binary tx, Arbitrary tx) => Blockchain tx -> Gen (Block tx)
arbitraryBlock' blks@((Block prevHeader _) :| rest) = do
    elapsed <- choose (9, 11)
    txs <- arbitrary :: Gen [tx]
    header <- pure BlockHeader
        { blockPreviousHash = blockHeaderHash prevHeader
        , blockRootHash     = hashTxs txs
        , blockDifficulty   = calculateDifficulty blks
        , blockTimestamp    = blockTimestamp prevHeader + elapsed
        , blockNonce        = 0
        }
    pure $ Block header (Seq.fromList txs)

arbitraryGenesis :: forall tx. (Binary tx, Arbitrary tx) => Gen (Block tx)
arbitraryGenesis = do
    txs <- resize 20 $ arbitrary :: Gen [tx]
    g <- genesisBlock <$> arbitrary <*> pure txs
    pure $ g { blockHeader = (blockHeader g) { blockDifficulty = minDifficulty } }

-- | Generate a *valid* arbitrary blockchain.
arbitraryBlockchain' :: forall tx. (Binary tx, Arbitrary tx) => Gen (Blockchain tx)
arbitraryBlockchain' = do
    gen <- arbitraryGenesis :: (Gen (Block tx))
    height <- choose (3, 4) :: Gen Int
    go (gen :| []) height
  where
    go blks 0 =
        pure blks
    go blks n = do
        blk <- arbitraryBlock' blks
        go (blk <| blks) (n - 1)
