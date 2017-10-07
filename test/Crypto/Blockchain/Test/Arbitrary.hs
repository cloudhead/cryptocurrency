{-# LANGUAGE ScopedTypeVariables #-}
module Crypto.Blockchain.Test.Arbitrary where

import           Crypto.Blockchain
import           Crypto.Blockchain.Block
import           Crypto.Hash

import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty((:|)), (<|))
import           Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Sequence as Seq
import           Data.Binary (Binary)

import           Test.QuickCheck

instance Arbitrary tx => Arbitrary (Block tx) where
    arbitrary =
        Block <$> arbitrary <*> arbitrary

instance Arbitrary tx => Arbitrary (Blockchain tx) where
    arbitrary = do
        genesis <- arbitraryGenesis
        rest <- arbitrary :: Gen [Block tx]
        pure $ genesis :| rest

instance Arbitrary (Digest SHA256) where
    arbitrary = do
        str <- arbitrary
        pure . fromJust $ digestFromByteString (Char8.pack str)

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

arbitraryGenesis :: forall tx. Arbitrary tx => Gen (Block tx)
arbitraryGenesis = do
    txs <- arbitrary :: Gen [tx]
    g <- pure $ genesisBlock txs
    pure $ g { blockHeader = (blockHeader g) { blockDifficulty = minDifficulty } }

-- | Generate a *valid* arbitrary blockchain.
arbitraryBlockchain' :: forall tx. (Binary tx, Arbitrary tx) => Gen (Blockchain tx)
arbitraryBlockchain' = do
    gen <- arbitraryGenesis :: (Gen (Block tx))
    height <- choose (32, 64) :: Gen Int
    go (gen :| []) height
  where
    go blks 0 =
        pure blks
    go (prevBlk :| rest) n = do
        blk <- arbitraryBlock' (prevBlk :| rest)
        go (blk :| prevBlk : rest) (n - 1)
