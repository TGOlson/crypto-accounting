module Data.Exchange.Coinbase
    ( CoinbaseRow(..)
    , toEvent
    ) where

import qualified Data.Csv             as CSV
import           Data.Maybe
import           GHC.Generics

import           Data.CSV.Instances   ()
import qualified Data.Currency.Crypto as Currency
import qualified Data.Currency.Fiat   as Currency
import           Data.Event.Types
import           Data.Exchange.Types

data CoinbaseRow = CoinbaseRow
    { _timestamp             :: String
    , _balance               :: Rational
    , _amount                :: Rational
    , _currency              :: Currency.Crypto
    , _to                    :: String
    , _notes                 :: Maybe String
    , _instantlyExchanged    :: String
    , _transferTotal         :: Maybe Rational
    , _transferTotalCurrency :: Maybe Currency.Fiat
    , _transferFee           :: Maybe Rational
    , _transferFeeCurrency   :: Maybe Currency.Fiat
    , _transferPaymentMethod :: Maybe String
    , _transferID            :: Maybe String
    , _orderPrice            :: Maybe String
    , _orderCurrency         :: Maybe String
    , _orderBTC              :: Maybe String
    , _orderTrackingCode     :: Maybe String
    , _orderCustomParameter  :: Maybe String
    , _orderPaidOut          :: Maybe String
    , _recurringPaymentID    :: Maybe String
    , _coinbaseID            :: String
    , _bitcoinHash           :: Maybe String
    }
  deriving (Generic, Show)

instance CSV.FromRecord CoinbaseRow

-- Note: Coinbase rows will never result in a trade event.
-- This is simply because Coinbase does not support crypto trading.
-- TODO: GDAX?
toEvent :: CoinbaseRow -> Event
toEvent row = if isPurchase
      then PurchaseEvent $
          Purchase timestamp Coinbase crypto amount currency price fee
      else TransferEvent $
          Transfer timestamp (ExchangeLocation Coinbase) PrivateWalletLocation crypto amount 0.0
  where
    isPurchase = isJust (_transferTotal row)
    timestamp  = _timestamp row
    crypto   = _currency row
    amount     = abs $ _amount row
    currency = get _transferTotalCurrency
    -- currency = Currency.USD
    price = get _transferTotal
    fee = get _transferFee
    -- fee = 0.0
    get fn = fromJust (fn row)
