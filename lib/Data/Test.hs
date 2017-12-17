module Data.Test
    ( main
    ) where

import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Csv               as CSV
import           Data.Maybe
import           Data.Monoid
import           Data.Vector

import           Data.Event
import qualified Data.Exchange.Coinbase as Coinbase
import qualified Data.Exchange.Gemini   as Gemini

main :: IO ()
-- main = testCoinbase
main = _testGemini

_testCoinbase :: IO ()
_testCoinbase = do
    bs <- LBS.readFile "./test_data/coinbase_edited.csv"

    let (rows :: Vector Coinbase.CoinbaseRow) = either (\e -> error $ "Unable to decode CSV: " <> e) id $ CSV.decode CSV.HasHeader bs
        events = catMaybes $ toList $ Coinbase.toEvent <$> rows
        lots = computeLots events
        tax = findTaxableEvents events

    print lots
    print tax

_testGemini :: IO ()
_testGemini = do
    bs <- LBS.readFile "./test_data/gemini.csv"

    let (rows :: Vector Gemini.GeminiRow) = either (\e -> error $ "Unable to decode CSV: " <> e) id $ CSV.decode CSV.HasHeader bs
        events = catMaybes $ toList $ Gemini.toEvent <$> rows
        lots = computeLots events
        tax = findTaxableEvents events
        holdings = computeHoldings lots

    print lots
    print tax
    print holdings
