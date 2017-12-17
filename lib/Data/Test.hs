module Data.Test
    ( main
    ) where

import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Csv               as CSV
import           Data.Monoid
import           Data.Vector

import           Data.Event
-- import           Data.Exchange
import           Data.Exchange.Coinbase
-- import           Data.Exchange.Gemini

main :: IO ()
main = do
    bs <- LBS.readFile "./test_data/coinbase_edited.csv"

    let (rows :: Vector CoinbaseRow) = either (\e -> error $ "Unable to decode CSV: " <> e) id $ CSV.decode CSV.HasHeader bs
        events = toList $ toEvent <$> rows
        lots = computeLots events
        tax = findTaxableEvents events

    print lots
    print tax


-- toEvent :: CoinbaseRow -> Event
-- toEvent row = if isPurchase
--     then PurchaseEvent Coinbase $
--         Purchase timestamp currency amount (get _transferTotal) (get _transferFee) (get _transferTotalCurrency)
--     else TransferEvent Coinbase $ Transfer timestamp currency amount (_to row)
--   where
--     isPurchase = isJust (_transferTotal row)
--     timestamp  = _timestamp (row :: CoinbaseRow)
--     currency   = _currency (row :: CoinbaseRow)
--     amount     = _amount (row :: CoinbaseRow)
--     get fn = fromJust (fn row)
