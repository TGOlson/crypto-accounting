module Data.Test
    ( main
    ) where

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Csv                   as CSV
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import qualified Data.Vector                as V


import           Data.Currency.Crypto
import           Data.Currency.Fiat
import           Data.Event
import qualified Data.Exchange.BittrexTrade as BittrexTrade
import qualified Data.Exchange.Coinbase     as Coinbase
import qualified Data.Exchange.Gemini       as Gemini
import           Data.Historical

main :: IO ()
-- main = getCurrentTime >>= \t -> getHistoricalPrice BTC USD t >>= print
-- main = _testCoinbase
-- main = _testGemini
main = _testBittrexTrade

_testCoinbase :: IO ()
_testCoinbase = do
    bs <- LBS.readFile "./test_data/coinbase_edited.csv"

    let rows = either (\e -> error $ "Unable to decode CSV: " <> e) id $ CSV.decode CSV.HasHeader bs
        events = catMaybes $ V.toList $ Coinbase.toEvent <$> rows
        lots = computeLots events
        tax = findTaxableEvents events

    print lots
    print tax

_testGemini :: IO ()
_testGemini = do
    bs <- LBS.readFile "./test_data/gemini.csv"

    let rows = either (\e -> error $ "Unable to decode CSV: " <> e) id $ CSV.decode CSV.HasHeader bs
        events = catMaybes $ V.toList $ Gemini.toEvent <$> rows
        lots = computeLots events
        tax = findTaxableEvents events
        holdings = computeHoldings lots

    print lots
    print tax
    print holdings


_testBittrexTrade :: IO ()
_testBittrexTrade = do
    bs <- LBS.readFile "./test_data/bittrex_trades_edited.csv"

    let rows = either (\e -> error $ "Unable to decode CSV: " <> e) id $ CSV.decode CSV.HasHeader bs

    eventsM <- sequence $ BittrexTrade.toEvent <$> V.toList rows

    let events = catMaybes eventsM
        lots = computeLots events
        tax = findTaxableEvents events
        holdings = computeHoldings lots

    print events
    print lots
    print tax
    print holdings
