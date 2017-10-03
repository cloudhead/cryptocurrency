module Bitcoin.Test.Arbitrary where

import Bitcoin
import Bitcoin.Tx
import Bitcoin.Types
import Bitcoin.Crypto

import Crypto.Blockchain.Test.Arbitrary
import Test.QuickCheck
import Data.ByteString

instance Arbitrary PublicKey where
    arbitrary = undefined

instance Arbitrary Signature where
    arbitrary = do
        bs <- arbitrary :: Gen ByteString
        pure bs

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

