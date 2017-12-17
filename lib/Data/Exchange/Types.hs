module Data.Exchange.Types
    ( Exchange(..)
    ) where

data Exchange
    = Coinbase
    | Gemini
    | Bittrex
  deriving (Eq, Show)
