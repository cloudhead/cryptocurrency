module Bitcoin.Tx where

import           Bitcoin.Types
import           Bitcoin.Crypto (PublicKey)

import           Crypto.Blockchain.Hash
import           Crypto.Hash (Digest, SHA256(..), HashAlgorithm, hashlazy, digestFromByteString, hashDigestSize)
import           Data.Binary (Binary, get, put, encode)
import           Data.Word (Word64, Word32)
import           Data.ByteString hiding (putStrLn)
import           Data.ByteString.Base58
import           Data.ByteString.Lazy (toStrict)
import           GHC.Generics (Generic)

-- | Amount in smallest denomination. Ex: Satoshis.
type Amount = Word64

type TxId = Digest SHA256
type TxInput = UTxOutput
type TxOutput = (Address, Amount)

newtype Address = Address ByteString
    deriving (Eq, Show, Binary)

toAddress :: PublicKey -> Address
toAddress pk = Address . encodeBase58 bitcoinAlphabet . toStrict $ encode pk

data UTxOutput = UTxOutput
    { utxoTxId   :: TxId
    , utxoIndex  :: Word32
    , utxoSig    :: Signature
    , utxoPubKey :: PublicKey
    } deriving (Eq, Show, Generic)

instance Binary UTxOutput

utxo :: Tx' -> Word32 -> UTxOutput
utxo tx vout =
    UTxOutput
        { utxoTxId   = txDigest tx
        , utxoIndex  = vout
        , utxoSig    = undefined
        , utxoPubKey = undefined
        }

txoAmount :: TxOutput -> Amount
txoAmount (_, amount) = amount

data Tx digest = Tx
    { txDigest    :: digest
    , txInputs    :: [UTxOutput]
    , txOutput    :: [TxOutput]
    } deriving (Eq, Show, Generic)

type Tx' = Tx (Digest SHA256)

transactionFee :: Tx a -> Amount
transactionFee = undefined

instance Binary a => Binary (Tx a)

transaction
    :: [TxInput]
    -> [TxOutput]
    -> ChainM Tx'
transaction ins outs =
    pure $ Tx digest ins outs
  where
    digest = hashlazy (encode tx)
    tx     = Tx () ins outs

coinbase
    :: [TxOutput]
    -> ChainM Tx'
coinbase outs =
    transaction [] outs
