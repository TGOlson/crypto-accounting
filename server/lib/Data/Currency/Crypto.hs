module Data.Currency.Crypto
    ( Crypto(..)
    ) where

import Data.Aeson
import Data.ByteString.Char8
import Data.Csv
import Data.Hashable
import GHC.Generics

data Crypto = BTC | ETH | LTC | Other String
  deriving (Eq, Generic, Show)

instance ToJSON Crypto
instance FromJSON Crypto

instance Hashable Crypto

-- TODO: more generic implementation
instance FromField Crypto where
    parseField "BTC" = pure BTC
    parseField "ETH" = pure ETH
    parseField "LTC" = pure LTC
    parseField x     = pure $ Other (unpack x)
    -- parseField xs    = fail $ "Unable to parse crypto from '" <> unpack xs <> "'"
