module Bitcoin.Test.Arbitrary where

import Bitcoin
import Bitcoin.Tx
import Bitcoin.Types
import Bitcoin.Crypto

import Crypto.Blockchain.Test.Arbitrary
import Test.QuickCheck
import Data.ByteString
import Crypto.Random

instance Arbitrary PublicKey where
    arbitrary = do
        [a, b, c, d, e] <- vectorOf 5 arbitrary
        let drg = drgNewTest (a, b, c, d, e)
        pure . fst . fst $ withDRG drg generateKeyPair

instance Arbitrary Address where
    arbitrary = do
        bs <- arbitrary
        pure $ Address bs

instance Arbitrary digest => Arbitrary (Tx digest) where
    arbitrary =
        Tx <$> arbitrary
           <*> arbitrary
           <*> arbitrary

instance Arbitrary UTxOutput where
    arbitrary =
        UTxOutput <$> arbitrary
                  <*> arbitrary
                  <*> arbitrary
                  <*> arbitrary

