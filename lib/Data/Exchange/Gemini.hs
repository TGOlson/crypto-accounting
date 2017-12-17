module Data.Exchange.Gemini
    ( GeminiRow(..)
    , toEvent
    ) where

import qualified Data.ByteString.Char8  as C8
import qualified Data.ByteString.Lazy   as L8
import qualified Data.ByteString.Search as BS
import           Data.Csv
import           Data.Maybe
import           Data.Monoid
-- import           Data.String.Utils
import           Data.Time
import           GHC.Generics
import           Safe

import           Data.CSV.Instances     ()
import qualified Data.Currency.Crypto   as Currency
import qualified Data.Currency.Fiat     as Currency
import           Data.Event.Types
import           Data.Exchange.Types
import           Data.Util.RationalF

data GeminiRow = GeminiRow
    { _date                  :: GeminiTimestamp
    , _time                  :: GeminiTimestamp
    , _type                  :: GeminiType
    , _symbol                :: GeminiSymbol
    , _specification         :: String
    , _liquidityIndicator    :: Maybe String
    , _tradingFeeRate        :: Maybe RationalF
    , _usdAmount             :: Maybe (GeminiAmount RationalF)
    , _tradingFeeUSD         :: Maybe (GeminiAmount RationalF)
    , _usdBalance            :: GeminiAmount RationalF
    , _btcAmount             :: Maybe (GeminiAmount RationalF)
    , _tradingFreeBTC        :: Maybe (GeminiAmount RationalF)
    , _btcBalance            :: GeminiAmount RationalF
    , _ethAmount             :: Maybe (GeminiAmount RationalF)
    , _ethBalance            :: GeminiAmount RationalF
    , _tradeId               :: Maybe Int
    , _orderId               :: Maybe Int
    , _orderDate             :: Maybe String
    , _orderTime             :: Maybe String
    , _clientOrderId         :: Maybe String
    , _apiSession            :: Maybe String
    , _txHash                :: Maybe String
    , _depositTxOutput       :: Maybe String
    , _withdrawalDestination :: Maybe String
    , _withdrawalTxOutput    :: Maybe RationalF
    }
  deriving (Generic, Show)

instance FromRecord GeminiRow

toEvent :: GeminiRow -> Maybe Event
toEvent row = case (_type row, _symbol row) of
    (Buy,    BTCUSD) -> Just $ PurchaseEvent $ Purchase ts Gemini Currency.BTC btcAmount Currency.USD usdAmount usdFeeAmount
    (Buy,    ETHUSD) -> Just $ PurchaseEvent $ Purchase ts Gemini Currency.ETH ethAmount Currency.USD usdAmount usdFeeAmount
    (Sell,   BTCUSD) -> Just $ SaleEvent     $ Sale     ts Gemini Currency.BTC btcAmount Currency.USD usdAmount usdFeeAmount
    (Sell,   ETHUSD) -> Just $ SaleEvent     $ Sale     ts Gemini Currency.ETH ethAmount Currency.USD usdAmount usdFeeAmount
    (Credit, BTC  )  -> Just $ TransferEvent $ Transfer ts PrivateWalletLocation (ExchangeLocation Gemini) Currency.BTC btcAmount 0.0
    (Credit, ETH  )  -> Just $ TransferEvent $ Transfer ts PrivateWalletLocation (ExchangeLocation Gemini) Currency.ETH ethAmount 0.0
    (Debit,  BTC  )  -> Just $ TransferEvent $ Transfer ts (ExchangeLocation Gemini) PrivateWalletLocation Currency.BTC btcAmount 0.0
    (Debit,  ETH  )  -> Just $ TransferEvent $ Transfer ts (ExchangeLocation Gemini) PrivateWalletLocation Currency.ETH ethAmount 0.0
    -- Note: credit/debit USD is the only expected case to skip
    -- anything else would be unexpected, could error/warn in those cases
    (_,      _    )  -> Nothing
  where
    ts           = _timestamp (_time row)
    usdAmount    = abs $ _amount (get _usdAmount)
    usdFeeAmount = abs $ _amount (get _tradingFeeUSD)
    btcAmount    = abs $ _amount (get _btcAmount)
    ethAmount    = abs $ _amount (get _ethAmount)
    get fn     = fromJust (fn row) -- TODO: be safer


-- parsing utils

parseWithRead :: Read a => String -> C8.ByteString -> Parser a
parseWithRead tag bs = maybe err pure $ readMay str
  where
    str = C8.unpack bs
    err = fail $ "Unable to parse " <> tag <> " from '" <> str <> "'"

newtype GeminiTimestamp = GeminiTimestamp { _timestamp :: UTCTime }
  deriving (Show)

instance FromField GeminiTimestamp where
    parseField bs = GeminiTimestamp <$> parseField formatted
      where
        formatted = L8.toStrict $ BS.replace "/" ("-" :: C8.ByteString) bs

data GeminiType = Buy | Sell | Credit | Debit
  deriving (Eq, Read, Show)

instance FromField GeminiType where
    parseField = parseWithRead "GeminiType"

data GeminiSymbol = USD | BTC | ETH | BTCUSD | ETHUSD
  deriving (Eq, Read, Show)

instance FromField GeminiSymbol where
    parseField = parseWithRead "GeminiSymbol"

newtype GeminiAmount a = GeminiAmount { _amount :: a }
  deriving (Eq, Read, Show)

instance FromField a => FromField (GeminiAmount a) where
    parseField bs = GeminiAmount <$> parseField trimmed
      where
        trimmed = C8.takeWhile (/= ' ') $ C8.dropWhile (\x -> x == '(' || x == '$') bs
