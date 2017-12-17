module Data.Event.Types
    ( Event(..)
    , Purchase(..)
    , Sale(..)
    , Transfer(..)
    , Trade(..)
    , TradeItem(..)
    , Location(..)
    ) where

import qualified Data.Currency.Crypto as Currency
import qualified Data.Currency.Fiat   as Currency
import           Data.Exchange.Types


data Event
    = PurchaseEvent Purchase
    | SaleEvent     Sale
    | TransferEvent Transfer
    | TradeEvent    Trade
  deriving (Eq, Show)


data Location = ExchangeLocation Exchange | PrivateWalletLocation | UnknownLocation
  deriving (Eq, Show)


data Purchase = Purchase
    { purchaseTimestamp :: String
    , purchaseExchange  :: Exchange
    , purchaseCrypto    :: Currency.Crypto
    , purchaseAmount    :: Rational
    , purchaseFiat      :: Currency.Fiat
    , purchasePrice     :: Rational
    , purchaseFee       :: Rational -- assume fee is always in FIAT
    }
  deriving (Eq, Show)


data Sale = Sale
    { saleTimestamp :: String
    , saleExchange  :: Exchange
    , saleCrypto    :: Currency.Crypto
    , saleAmount    :: Rational
    , saleFiat      :: Currency.Fiat
    , salePrice     :: Rational
    , saleFee       :: Rational -- assume fee is always in FIAT
    }
  deriving (Eq, Show)


data Transfer = Transfer
    { transferTimestamp :: String
    , transferFrom      :: Location
    , transferTo        :: Location
    , transferCrypto    :: Currency.Crypto
    , transferAmount    :: Rational
    , transferFee       :: Rational
    -- , transferAddress   :: String -- TODO: might be nice to have to private wallets
    }
  deriving (Eq, Show)

data TradeItem = TradeItem
    { tradeItemCrypto          :: Currency.Crypto
    , tradeItemAmount          :: Rational
    , tradeItemUnderlyingValue :: Rational
    }
  deriving (Eq, Show)

data Trade = Trade
    { tradeTimestamp               :: String
    , tradeExchange                :: Exchange
    , tradeFromCrypto              :: Currency.Crypto
    , tradeFromAmount              :: Rational
    , tradeToCrypto                :: Currency.Crypto
    , tradeToAmount                :: Rational
    , tradeFee                     :: Rational -- Assume fee is always against `to` crypto type, for now
    , tradeItemUnderlyingValueFiat :: Currency.Fiat
    , tradeItemUnderlyingValue     :: Rational
    }
  deriving (Eq, Show)
