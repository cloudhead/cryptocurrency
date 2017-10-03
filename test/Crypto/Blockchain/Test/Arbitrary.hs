{-# LANGUAGE ScopedTypeVariables #-}
module Crypto.Blockchain.Test.Arbitrary where

import           Crypto.Blockchain
import           Crypto.Blockchain.Block
import           Crypto.Hash

import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty((:|)), (<|))
import           Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as Char8

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

arbitraryGenesis :: forall tx. Arbitrary tx => Gen (Block tx)
arbitraryGenesis = do
    txs <- arbitrary :: Gen [tx]
    pure $ genesisBlock txs
