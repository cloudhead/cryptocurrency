module Crypto.Blockchain.Types where

newtype Error = Error String

class Validate a where
    -- TODO: Consider making this
    -- a -> Either Error (Valid a)
    validate :: a -> Either Error a

