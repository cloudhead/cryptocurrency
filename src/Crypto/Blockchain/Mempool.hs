module Crypto.Blockchain.Mempool where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Foldable (toList)

newtype Mempool tx = Mempool { fromMempool :: Set tx }
    deriving (Show, Monoid)

addTx :: Ord tx => tx -> Mempool tx -> Mempool tx
addTx tx (Mempool txs) = Mempool (Set.insert tx txs)

addTxs :: (Ord tx, Foldable t) => t tx -> Mempool tx -> Mempool tx
addTxs txs' (Mempool txs) =
    Mempool $ Set.union txs (Set.fromList (toList txs'))

removeTxs :: (Ord tx, Foldable t) => t tx -> Mempool tx -> Mempool tx
removeTxs txs' (Mempool txs) =
    Mempool $ Set.difference (Set.fromList (toList txs')) txs
