{-# LANGUAGE NamedFieldPuns #-}
module Bitcoin.Tests where

import Bitcoin.Network
import Bitcoin.Crypto
import Bitcoin.Tx
import Crypto.Blockchain.Message (Message)
import Crypto.Blockchain.Block
import Crypto.Blockchain
import qualified Crypto.Blockchain.Message as Message
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad
import Data.Either (isRight)

tests :: TestTree
tests = testGroup "Bitcoin tests"
    [ testCase "Bitcoin" testBitcoin
    , testCase "Blockchain" testBlockchain ]

testBlockchain :: Assertion
testBlockchain = do
    alice@(pk, sk) <- generateKeyPair
    bob@(pk', sk') <- generateKeyPair

    chain <- pure $ do
        cb  <- coinbase [(toAddress pk, 1000)]
        gen <- genesisBlock [cb]
        blk <- block <=< sequence $
            [ transaction [utxo cb 0]
                [ (toAddress pk', 600)
                , (toAddress pk,  400)
                ]
            ]
        blockchain [gen, blk]

    isRight chain @? "Blockchain is valid"

testBitcoin :: Assertion
testBitcoin = do
    nodes <- createTestNetwork ["9999", "9998", "9997"] :: IO (TestNetwork (Message Tx'))

    let Node { nodeSocket } = head nodes

    broadcast nodeSocket Message.Ping
    pings <- forM (tail nodes) $ \Node { nodeName, nodeSocket } -> do
        msg <- receive nodeSocket
        pure (nodeName, msg)

    assertEqual "Nodes received pings" [("9998", Message.Ping), ("9997", Message.Ping)] pings
