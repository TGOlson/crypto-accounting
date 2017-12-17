module Data.Currency.Crypto
    ( Crypto(..)
    ) where

import Data.ByteString.Char8
import Data.Csv
import Data.Hashable
import Data.Monoid
import GHC.Generics

data Crypto = BTC | ETH | LTC
  deriving (Eq, Generic, Show)

instance Hashable Crypto

-- TODO: more generic implementation
instance FromField Crypto where
    parseField "BTC" = pure BTC
    parseField "ETH" = pure ETH
    parseField "LTC" = pure LTC
    parseField xs    = fail $ "Unable to parse crypto from '" <> unpack xs <> "'"
