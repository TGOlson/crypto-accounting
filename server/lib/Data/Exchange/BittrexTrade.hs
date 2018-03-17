module Data.Exchange.BittrexTrade
    ( BittrexTradeRow(..)
    , toEvent
    ) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Csv
import           Data.Monoid
import           Data.Time
import           GHC.Generics
import           Safe

import           Data.CSV.Instances    ()
import qualified Data.Currency.Crypto  as Currency
import qualified Data.Currency.Fiat    as Currency
import           Data.Event.Types
import           Data.Exchange.Types
import           Data.Historical
import           Data.Util.RationalF

data BittrexTradeRow = BittrexTradeRow
    { _orderUuid      :: String
    , _exchange       :: BittrexTradePair
    , _type           :: BittrexTradeType
    , _quantity       :: RationalF
    , _limit          :: RationalF
    , _commissionPaid :: RationalF
    , _price          :: RationalF
    , _opened         :: BittrexTradeTimestamp
    , _closed         :: BittrexTradeTimestamp
    }
  deriving (Generic, Show)

instance FromRecord BittrexTradeRow

toEvent :: BittrexTradeRow -> IO (Maybe Event)
toEvent row = do
    underlyingPerUnitEither <- getHistoricalPrice fromCrypto Currency.USD time

    let underlyingPerUnit = either (\x -> error $ "unable to fetch historical price " <> x) id underlyingPerUnitEither
        underlyingPrice   = underlyingPerUnit * amountFrom
        trade             = Trade time Bittrex fromCrypto amountFrom toCrypto amountTo fee Currency.USD underlyingPrice

    return $ Just (TradeEvent trade)
  where
    time       = _timestamp (_closed row)
    (c1, c2)   = _pair (_exchange row)
    quantity   = _quantity row
    price      = _price row
    fee        = 0.0 -- Bittrex has fee info, Bittrex has fee info, but it not always the `to` crypto
    (amountFrom, amountTo) = if _type row == LIMIT_BUY then (price, quantity) else (quantity, price)
    (fromCrypto, toCrypto) = if _type row == LIMIT_BUY then (c1, c2) else (c2, c1)


-- parsing utils

parseWithRead :: Read a => String -> C8.ByteString -> Parser a
parseWithRead tag bs = maybe err pure $ readMay str
  where
    str = C8.unpack bs
    err = fail $ "Unable to parse " <> tag <> " from '" <> str <> "'"

newtype BittrexTradeTimestamp = BittrexTradeTimestamp { _timestamp :: UTCTime }
  deriving (Show)

instance FromField BittrexTradeTimestamp where
    parseField bs = BittrexTradeTimestamp <$> utcTimeP
      where
        [date, time, tag] = C8.split ' ' bs
        [month, day, year] = C8.split '/' date
        [hour, minute, sec] = C8.split ':' time
        hourAdjusted =
            let adj = if tag == "PM" then 12 else 0
            in C8.pack $ show $ (read (C8.unpack hour) :: Int) + adj
        dateString = year <> "-" <> pad month <> "-" <> pad day <> " " <> pad hourAdjusted <> ":" <> minute <> ":" <> sec
        utcTimeP = parseField dateString

        pad x  = if BS.length x == 1 then "0" <> x else x


data BittrexTradeType = LIMIT_SELL | LIMIT_BUY | Credit | Debit
  deriving (Eq, Read, Show)

instance FromField BittrexTradeType where
    parseField = parseWithRead "BittrexTradeType"

newtype BittrexTradePair = BittrexTradePair { _pair :: (Currency.Crypto, Currency.Crypto)}
  deriving (Eq, Show)

instance FromField BittrexTradePair where
    parseField bs = do
        p1F <- parseField (bchHack p1)
        p2F <- parseField (bchHack p2)

        return $ BittrexTradePair (p1F, p2F)
      where
        [p1, p2] = C8.split '-' bs
        bchHack = \case "BCC" -> "BCH"
                        x     -> x
