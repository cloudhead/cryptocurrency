{-# LANGUAGE TypeApplications #-}
module Bitcoin.Crypto
    ( generateKeyPair
    , module Crypto.PubKey.ECC.ECDSA
    ) where

import Crypto.PubKey.ECC.Generate (generate)
import Crypto.PubKey.ECC.ECDSA (PublicKey(..), PrivateKey)
import Crypto.PubKey.ECC.Types (CurveName(SEC_p256k1), Point(..), getCurveByName)
import Crypto.Random.Types (MonadRandom)

import Data.Binary
import Data.ByteString

instance Binary PublicKey where
    put (PublicKey curve (Point x y)) | curve == getCurveByName SEC_p256k1 =
        put x >> put y
    put _ =
        error "can't encode PublicKey curve or point type"
    get = do
        x <- get @Integer
        y <- get @Integer

        pure $ PublicKey (getCurveByName SEC_p256k1) (Point x y)

generateKeyPair :: MonadRandom m => m (PublicKey, PrivateKey)
generateKeyPair = do
    (pk, sk) <- generate (getCurveByName SEC_p256k1)
    pure (pk, sk)

