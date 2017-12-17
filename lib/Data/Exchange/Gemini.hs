module Data.Exchange.Gemini
    ( GeminiRow(..)
    ) where

import qualified Data.Csv     as CSV
import           GHC.Generics


data GeminiRow = GeminiRow
    { _date                  :: String
    , _time                  :: String
    , _type                  :: String
    , _symbol                :: String
    , _specification         :: String
    , _liquidityIndicator    :: Maybe String
    , _tradingFeeRate        :: Maybe Double
    , _usdAmount             :: Maybe String -- TODO: parse signed double
    , _tradingFeeUSD         :: Maybe String -- TODO: parse signed double
    , _usdBalance            :: String
    , _btcAmount             :: Maybe String -- TODO: parse signed double
    , _tradingFreeBTC        :: Maybe String -- TODO: parse signed double
    , _btcBalance            :: String -- TODO: remove trailing "BTC" units
    , _ethAmount             :: Maybe String -- TODO: parse signed double
    , _ethBalance            :: String -- TODO: remove trailing "ETH" units
    , _tradeId               :: Maybe Int
    , _orderId               :: Maybe Int
    , _orderDate             :: Maybe String
    , _orderTime             :: Maybe String
    , _clientOrderId         :: Maybe String
    , _apiSession            :: Maybe String
    , _txHash                :: Maybe String
    , _depositTxOutput       :: Maybe String
    , _withdrawalDestination :: Maybe String
    , _withdrawalTxOutput    :: Maybe Double
    }
  deriving (Generic, Show)

instance CSV.FromRecord GeminiRow
