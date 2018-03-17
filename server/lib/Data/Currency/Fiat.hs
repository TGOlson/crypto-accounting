module Data.Currency.Fiat
    ( Fiat(..)
    ) where

import Data.Aeson
import Data.ByteString.Char8
import Data.Csv
import Data.Monoid
import GHC.Generics

data Fiat = USD | NA
  deriving (Eq, Generic, Show)

instance ToJSON Fiat
instance FromJSON Fiat

-- TODO: more generic implementation
instance FromField Fiat where
    parseField "USD" = pure USD
    parseField xs    = fail $ "Unable to parse currency from '" <> unpack xs <> "'"
