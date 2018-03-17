module Data.Exchange.Types
    ( Exchange(..)
    ) where

import Data.Aeson
import GHC.Generics

data Exchange
    = Coinbase
    | Gemini
    | Bittrex
  deriving (Eq, Generic, Show)

instance ToJSON Exchange
instance FromJSON Exchange
