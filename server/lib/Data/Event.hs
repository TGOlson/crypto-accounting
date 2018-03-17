module Data.Event
    ( Event(..)
    , Purchase(..)
    , Sale(..)
    , Transfer(..)
    , Trade(..)
    , TaxableEvent(..)
    , Lot(..)
    , Location(..)
    -- , getHoldings
    , computeLots
    , computeHoldings
    , findTaxableEvents
    , findLotSplits
    ) where

import           Control.Arrow
import           Data.Aeson
import           Data.List
import           Data.Time
import           GHC.Generics         hiding (from, to)

import qualified Data.Currency.Crypto as Currency
import qualified Data.Currency.Fiat   as Currency
import           Data.Event.Types
import qualified Data.HashMap.Strict  as H
import           Data.Util.RationalF


data TaxableEvent = TaxableEvent
    { taxableEventCurrency   :: Currency.Fiat
    , taxableEventNet        :: RationalF
    , taxableEventUnderlying :: Event
    , taxableEventLotsUsed   :: [Lot]
    }
  deriving (Eq, Generic, Show)

instance ToJSON TaxableEvent

-- * get events
-- * sort by timestamp
-- * iterate through
--   * if purchase
--     * add to lots by exchange (only coinbase/gemini)
--     * calculate cost basis and cost basis per share
--     * use fee to increase cost basis
--   * if sell
--     * deduct from lots by exchange
--     * use specified lot picking strategy (fifo, pickbest)
--     * use fee to reduce proceeds of sale
--   * if transfer
--     * reduce value on exchange
--     * inc value in "private wallet" (do not track addresses for now)
--     * retain attatched cost basis from purchase (moving lots, not sums)
--   * if trade
--     * reduce lots using specified lot picking strategy (fifo, pickbest)
--     * use fee to reduce cost basis
--     * calculate cost basis and cost basis per share

data Lot = Lot
    { lotTime      :: UTCTime
    , lotCrypto    :: Currency.Crypto
    , lotAmount    :: RationalF
    , lotCostBasis :: RationalF
    , lotLocation  :: Location
    }
  deriving (Eq, Generic, Show)

instance ToJSON Lot

computeLots :: [Event] -> [Lot]
computeLots = fst . foldl findTaxableEventWithLots mempty

computeHoldings :: [Lot] -> H.HashMap Currency.Crypto RationalF
computeHoldings lots = H.fromListWith (+) amounts
  where
    amounts = (lotCrypto &&& lotAmount) <$> lots

findTaxableEvents :: [Event] -> [TaxableEvent]
findTaxableEvents = snd . foldl findTaxableEventWithLots mempty

findTaxableEventWithLots :: ([Lot], [TaxableEvent]) -> Event -> ([Lot], [TaxableEvent])
findTaxableEventWithLots (lots, taxEvents) event =
    case event of
        PurchaseEvent (Purchase ts ex crypto amount _fiat price fee) ->
            let lot = Lot ts crypto amount (price + fee) (ExchangeLocation ex)
            in (lots ++ [lot], taxEvents)
        SaleEvent (Sale _ts ex crypto amount fiat price fee) ->
            let (usedLots, remainingLots) = findLotSplits (ExchangeLocation ex) crypto amount lots
                totalCostBasis = sum (lotCostBasis <$> usedLots)
                net = price - fee - totalCostBasis
                taxableEvent = TaxableEvent fiat net event usedLots
            in (remainingLots, taxableEvent : taxEvents)
        TransferEvent (Transfer ts from to crypto amount fee) ->
            let (usedLots, remainingLots) = findLotSplits from crypto amount lots
                totalTransferred = sum $ lotAmount <$> usedLots
                updatedUsedLots = (\l -> l {lotLocation = to, lotAmount = lotAmount l - (lotAmount l / totalTransferred * fee)  }) <$> usedLots
                -- if there is no previous lot for the transfer, create a new lot with a cost basis of zero
                -- later on, this should be tagged with a note stating the inconsistency so that a user could update the lot
                updatedUsedLots' = if null updatedUsedLots then pure $ Lot ts crypto (amount  - fee) 0.0 to else updatedUsedLots

            -- TODO: should these lots be appended?
            -- Lot might need a timestamp of when they were created
            in (updatedUsedLots' ++ remainingLots, taxEvents)
        TradeEvent (Trade ts ex fromCrypto fromAmount toCrypto toAmount fee fiat underlyingValue) ->
            let (usedLots, remainingLots) = findLotSplits (ExchangeLocation ex) fromCrypto fromAmount lots
                totalCostBasis = sum (lotCostBasis <$> usedLots)
                net = underlyingValue - totalCostBasis
                taxableEvent = TaxableEvent fiat net event usedLots
                newLot = Lot ts toCrypto (toAmount - fee) underlyingValue (ExchangeLocation ex)
                -- TODO: register this trade as a tax event
            in (newLot : remainingLots, taxableEvent : taxEvents)

-- data LotDetails = LotDetails Lot | UnknownLot

-- TODO: lot picking strategy
-- right now this uses FIFO
-- TODO: UnknownLot location when we can't find a lot in the bunch
-- TODO: Lots should be :: Map Location (Map Crypto RationalF)
findLotSplits :: Location -> Currency.Crypto -> RationalF -> [Lot] -> ([Lot], [Lot])
findLotSplits loc crypto amount lots = (\(x, y, _z) -> (reverse x, reverse y)) $ foldl moveLot (mempty, mempty, 0.0) (sortOn lotTime lots) -- TODO: better lot sorting strategy
  where
    moveLot :: ([Lot], [Lot], RationalF) -> Lot -> ([Lot], [Lot], RationalF)
    moveLot (usedLots, unusedLots, used) lot | used == amount        = (usedLots, lot : unusedLots, used)
    moveLot (usedLots, unusedLots, used) lot | not (isUsableLot lot) = (usedLots, lot : unusedLots, used)
    moveLot (usedLots, unusedLots, used) lot =
        let needed    = amount - used
            available = lotAmount lot
        in
        if available <= needed
            then (lot : usedLots, unusedLots, lotAmount lot + used)
            else
                let fullCostBasis = lotCostBasis lot
                    usedCostBasis = needed / available * fullCostBasis
                    leftCostBasis = (available - needed) / available * fullCostBasis
                    partialUsed   = lot { lotAmount = needed, lotCostBasis = usedCostBasis }
                    partialLeft   = lot { lotAmount = available - needed, lotCostBasis = leftCostBasis }
                in (partialUsed : usedLots, partialLeft : unusedLots, needed + used)

    isUsableLot (Lot _ c _ _ l) = c == crypto && l == loc

-- data EventResult = EventResult
--     { lotsUsed     :: [Lot]
--     , lotsCreated  :: [Lot]
--     , taxableEvent :: Maybe TaxableEvent
--     }

-- TODO: refactor to this after implementing trades
_applyEvent :: [Lot] -> Event -> ([Lot], Maybe TaxableEvent)
_applyEvent lots = \case
    PurchaseEvent ev -> (_applyPurchase lots ev, Nothing)
    SaleEvent     ev -> second pure (_applySale lots ev)
    TradeEvent    ev -> second pure (_applyTrade lots ev)
    TransferEvent ev -> (_applyTransfer lots ev, Nothing)


_applyPurchase :: [Lot] -> Purchase -> [Lot]
_applyPurchase = undefined

_applySale :: [Lot] -> Sale -> ([Lot], TaxableEvent)
_applySale = undefined

_applyTrade :: [Lot] -> Trade -> ([Lot], TaxableEvent)
_applyTrade = undefined

_applyTransfer :: [Lot] -> Transfer -> [Lot]
_applyTransfer = undefined
