module Main (main) where

import           Bitcoin.Tests
import           Test.Tasty

main :: IO ()
main = defaultMain tests
