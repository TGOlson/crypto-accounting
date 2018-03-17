module Data.Historical
    ( getHistoricalPrice
    ) where

import           Control.Lens
import qualified Data.HashMap.Strict   as H
import           Data.Monoid
import           Data.Time
import           Data.Time.Clock.POSIX
import           Network.Wreq

import qualified Data.Currency.Crypto  as Currency
import qualified Data.Currency.Fiat    as Currency
import qualified Data.Text             as T
import           Data.Util.RationalF

-- data HistoricalResponse = {}


getHistoricalPrice :: Currency.Crypto -> Currency.Fiat -> UTCTime -> IO (Either String RationalF)
getHistoricalPrice crypto fiat time = do
    -- TODO: data type for this response?
    r <- asJSON =<< getWith opts baseUrl
    let body = (r ^. responseBody) :: H.HashMap T.Text (H.HashMap T.Text Double)
        priceD = H.lookup cryptoT body >>= H.lookup fiatT
        price = (RationalF . toRational) <$> priceD

    return $ maybe (Left $ "Unable to parse response: " <> show r) Right price
  where
    -- Example: https://min-api.cryptocompare.com/data/pricehistorical?fsym=BTC&tsyms=USD&ts=1452680400
    -- TODO: query params data type
    cryptoT = case crypto of
        Currency.Other x -> T.pack x
        x                -> showT x
    fiatT = showT fiat
    timestampT = showT (floor (utcTimeToPOSIXSeconds time) :: Integer)
    opts = defaults &
        param "fsym"  .~ [cryptoT] &
        param "tsyms" .~ [fiatT] &
        param "ts"    .~ [timestampT]
    baseUrl = "https://min-api.cryptocompare.com/data/pricehistorical"
    showT :: Show a => a -> T.Text
    showT = T.pack . show
