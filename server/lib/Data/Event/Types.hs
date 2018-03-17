module Data.Event.Types
    ( Event(..)
    , Purchase(..)
    , Sale(..)
    , Transfer(..)
    , Trade(..)
    , Location(..)
    ) where

import           Data.Aeson
import           Data.Time
import           GHC.Generics

import qualified Data.Currency.Crypto as Currency
import qualified Data.Currency.Fiat   as Currency
import           Data.Exchange.Types
import           Data.Util.RationalF


data Event
    = PurchaseEvent Purchase
    | SaleEvent     Sale
    | TransferEvent Transfer
    | TradeEvent    Trade
  deriving (Eq, Generic, Show)

instance ToJSON Event
instance FromJSON Event


data Location = ExchangeLocation Exchange | PrivateWalletLocation | UnknownLocation
  deriving (Eq, Generic, Show)

instance ToJSON Location
instance FromJSON Location

data Purchase = Purchase
    { purchaseTime     :: UTCTime
    , purchaseExchange :: Exchange
    , purchaseCrypto   :: Currency.Crypto
    , purchaseAmount   :: RationalF
    , purchaseFiat     :: Currency.Fiat
    , purchasePrice    :: RationalF
    , purchaseFee      :: RationalF -- assume fee is always in FIAT
    }
  deriving (Eq, Generic, Show)

instance ToJSON Purchase
instance FromJSON Purchase

data Sale = Sale
    { saleTime     :: UTCTime
    , saleExchange :: Exchange
    , saleCrypto   :: Currency.Crypto
    , saleAmount   :: RationalF
    , saleFiat     :: Currency.Fiat
    , salePrice    :: RationalF
    , saleFee      :: RationalF -- assume fee is always in FIAT
    }
  deriving (Eq, Generic, Show)

instance ToJSON Sale
instance FromJSON Sale

data Transfer = Transfer
    { transferTime   :: UTCTime
    , transferFrom   :: Location
    , transferTo     :: Location
    , transferCrypto :: Currency.Crypto
    , transferAmount :: RationalF
    , transferFee    :: RationalF
    -- , transferAddress   :: String -- TODO: might be nice to have to private wallets
    }
  deriving (Eq, Generic, Show)

instance ToJSON Transfer
instance FromJSON Transfer

data Trade = Trade
    { tradeTime                    :: UTCTime
    , tradeExchange                :: Exchange
    , tradeFromCrypto              :: Currency.Crypto
    , tradeFromAmount              :: RationalF
    , tradeToCrypto                :: Currency.Crypto
    , tradeToAmount                :: RationalF
    , tradeFee                     :: RationalF -- Assume fee is always against `to` crypto type, for now
    , tradeItemUnderlyingValueFiat :: Currency.Fiat
    , tradeItemUnderlyingValue     :: RationalF
    }
  deriving (Eq, Generic, Show)

instance ToJSON Trade
instance FromJSON Trade
