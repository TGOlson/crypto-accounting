module Data.Currency.Fiat
    ( Fiat(..)
    ) where

import Data.ByteString.Char8
import Data.Csv
import Data.Monoid

data Fiat = USD
  deriving (Eq, Show)

-- TODO: more generic implementation
instance FromField Fiat where
    parseField "USD" = pure USD
    parseField xs    = fail $ "Unable to parse currency from '" <> unpack xs <> "'"
