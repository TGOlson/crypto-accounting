module Data.ExchangeSpec (spec) where

import Test.Hspec

import Data.Currency.Crypto
import Data.Currency.Fiat
import Data.Event
import Data.Exchange.Types
-- import Data.HashMap.Strict

spec :: Spec
spec =
    describe "Data.Exchange" $ do
        -- describe "getHoldings" $ do
        --     it "should handle an empty event list" $
        --         shouldBe (getHoldings mempty) mempty
        --
        --     it "should add two purchases" $
        --         shouldBe (getHoldings
        --             [ PurchaseEvent Coinbase $ Purchase "123" BTC 1.0 USD 1000.0 1.0
        --             , PurchaseEvent Gemini   $ Purchase "123" BTC 0.5 USD 700.0  1.0
        --             ])
        --             $ fromList [(BTC, 1.5)]
        --
        --     it "should combine a purchase and a sale" $
        --         shouldBe (getHoldings
        --             [ PurchaseEvent Coinbase $ Purchase "123" BTC 1.0 USD 1000.0 1.0
        --             , SaleEvent     Coinbase $ Sale     "123" BTC 0.8 USD 1100.0 1.0
        --             ])
        --             $ fromList [(BTC, 0.2)]
        --
        --     it "should combine a purchase and a trade" $
        --         shouldBe (getHoldings
        --             [ PurchaseEvent Coinbase $ Purchase "123" BTC 1.0 USD 1000.0 1.0
        --             , TradeEvent    Coinbase $ Trade    "123" BTC 0.2 ETH 10.0   1.0
        --             ])
        --             $ fromList [(BTC, 0.8), (ETH, 10.0)]

        describe "findTaxableEvents" $ do
            it "should handle an empty event list" $
                shouldBe (findTaxableEvents mempty) mempty

            it "should not find taxable events with only purchases" $
                shouldBe (findTaxableEvents
                    [ PurchaseEvent $ Purchase "123" Coinbase BTC 1.0 USD 1000.0 1.0
                    , PurchaseEvent $ Purchase "123" Coinbase BTC 0.5 USD 700.0  1.0
                    ])
                    mempty

            it "should find a taxable event when a purchase is present" $
                let pe1 = PurchaseEvent $ Purchase "123" Coinbase BTC 1.0 USD 1000.0 1.0
                    se1 = SaleEvent     $ Sale     "456" Coinbase BTC 0.8 USD 1100.0 1.0
                in shouldBe (findTaxableEvents [pe1, se1])
                    [ TaxableEvent "456" USD 298.2 se1 [ Lot BTC 0.8 800.8 (ExchangeLocation Coinbase) ]
                    ]

            it "should find a taxable event using multiple lots" $
                let pe1 = PurchaseEvent $ Purchase "123" Coinbase BTC 1.0 USD 1000.0 1.0
                    pe2 = PurchaseEvent $ Purchase "234" Coinbase BTC 0.5 USD  600.0 1.0
                    se1 = SaleEvent     $ Sale     "456" Coinbase BTC 1.2 USD 1400.0 1.0
                in shouldBe (findTaxableEvents [pe1, pe2, se1])
                    [ TaxableEvent "456" USD 157.6 se1
                        [ Lot BTC 1.0 1001.0 (ExchangeLocation Coinbase)
                        , Lot BTC 0.2  240.4 (ExchangeLocation Coinbase)
                        ]
                    ]

            it "should use a cost basis of zero when no previous lots are present" $
                let se1 = SaleEvent $ Sale "456" Coinbase BTC 0.8 USD 1100.0 1.0
                in shouldBe (findTaxableEvents [se1])
                    [ TaxableEvent "456" USD 1099.0 se1 mempty ]

            it "should find a taxable event from a trade" $
                let pe1 = PurchaseEvent $ Purchase "123" Coinbase BTC 1.0 USD 1000.0 1.0
                    te1 = TradeEvent    $ Trade    "456" Coinbase BTC 1.0 ETH 10.0 0.1 USD 1200.00
                in shouldBe (findTaxableEvents [pe1, te1])
                    [ TaxableEvent "456" USD 199.0 te1
                        [ Lot BTC 1.0 1001.0 (ExchangeLocation Coinbase) ]
                    ]

             -- it "should change the location of lots during a transfer event" $
            --     let pe1 = PurchaseEvent $ Purchase "123" Coinbase BTC 1.0 USD 1000.0 1.0
            --         pe2 = PurchaseEvent $ Purchase "234" Coinbase BTC 0.5 USD  600.0 1.0
            --         te1 = TransferEvent $ Transfer "456" (ExchangeLocation Coinbase) PrivateWalletLocation BTC 1.2 0.01
            --         te2 = TransferEvent $ Transfer "567" PrivateWalletLocation (ExchangeLocation Gemini) BTC 1.0 0.01
            --         se1 = SaleEvent     $ Sale     "890" Gemini BTC 0.5 USD 900.0 1.0
            --     in
            --     shouldBe (findTaxableEvents [pe1, pe2, te1, te2, se1])
            --         [ TaxableEvent "456" USD 157.6 te1
            --             [ Lot BTC 1.0 1001.0 (ExchangeLocation Gemini)
            --             , Lot BTC 0.2  240.4 (ExchangeLocation Coinbase)
            --             ]
            --         ]

        describe "computeLots" $ do
            it "should handle an empty event list" $
                shouldBe (computeLots mempty) mempty

            it "should find lots with only purchases" $
                shouldBe (computeLots
                    [ PurchaseEvent $ Purchase "123" Coinbase BTC 1.0 USD 1000.0 1.0
                    , PurchaseEvent $ Purchase "123" Coinbase BTC 0.5 USD 700.0  1.0
                    ])
                    [ Lot BTC 1.0 1001.0 (ExchangeLocation Coinbase)
                    , Lot BTC 0.5  701.0 (ExchangeLocation Coinbase)
                    ]

            it "should remove lots during sales" $
                shouldBe (computeLots
                    [ PurchaseEvent $ Purchase "123" Coinbase BTC 1.0 USD 1000.0 1.0
                    , SaleEvent     $ Sale     "456" Coinbase BTC 0.8 USD 1100.0 1.0
                    ])
                    [ Lot BTC 0.2 200.2 (ExchangeLocation Coinbase) ]

            -- Note: this test assume FIFO lot picking strategy
            it "should use multiple lots to make a sale" $
                shouldBe (computeLots
                    [ PurchaseEvent $ Purchase "123" Coinbase BTC 1.0 USD 1000.0 1.0
                    , PurchaseEvent $ Purchase "234" Coinbase BTC 0.5 USD  600.0 1.0
                    , SaleEvent     $ Sale     "456" Coinbase BTC 1.2 USD 1400.0 1.0
                    ])
                    [ Lot BTC 0.3 360.6 (ExchangeLocation Coinbase) ]

            it "should change location when a entire lot is transferred" $
                shouldBe (computeLots
                    [ PurchaseEvent $ Purchase "123" Coinbase BTC 1.0 USD 1000.0 1.0
                    , TransferEvent $ Transfer "456" (ExchangeLocation Coinbase) PrivateWalletLocation BTC 1.0 0.01
                    ])
                    [ Lot BTC 0.99 1001.0 PrivateWalletLocation ]

            it "should change location when a partial lot is transferred" $
                shouldBe (computeLots
                    [ PurchaseEvent $ Purchase "123" Coinbase BTC 1.0 USD 1000.0 1.0
                    , TransferEvent $ Transfer "456" (ExchangeLocation Coinbase) PrivateWalletLocation BTC 0.5 0.01
                    ])
                    [ Lot BTC 0.49 500.5 PrivateWalletLocation
                    , Lot BTC 0.50 500.5 (ExchangeLocation Coinbase)
                    ]

            it "should use multiple lots to make a transfer" $
                shouldBe (computeLots
                    [ PurchaseEvent $ Purchase "123" Coinbase BTC 1.0 USD 1000.0 1.0
                    , PurchaseEvent $ Purchase "234" Coinbase BTC 0.5 USD  600.0 1.0
                    , TransferEvent $ Transfer "456" (ExchangeLocation Coinbase) PrivateWalletLocation BTC 1.2 0.01
                    ])
                    [ Lot BTC (119 / 120) 1001.0 PrivateWalletLocation
                    , Lot BTC (119 / 600)  240.4 PrivateWalletLocation
                    , Lot BTC 0.3    360.6 (ExchangeLocation Coinbase)
                    ]

            it "should create a new lot if no previous lot can be found for transfer" $
                shouldBe (computeLots
                    [ TransferEvent $ Transfer "456" (ExchangeLocation Coinbase) PrivateWalletLocation BTC 1.0 0.01 ])
                    [ Lot BTC 0.99 0.0 PrivateWalletLocation ]

            it "should update holdings on a complete trade" $
                shouldBe (computeLots
                    [ PurchaseEvent $ Purchase "123" Coinbase BTC 1.0 USD 1000.0 1.0
                    , TradeEvent    $ Trade    "456" Coinbase BTC 1.0 ETH 10.0 0.1 USD 1200.00
                    ])
                    [ Lot ETH 9.9 1200.0 (ExchangeLocation Coinbase) ]

            it "should update holdings using multiple lots" $
                shouldBe (computeLots
                    [ PurchaseEvent $ Purchase "123" Coinbase BTC 1.0 USD 1000.0 1.0
                    , PurchaseEvent $ Purchase "234" Coinbase BTC 0.5 USD  600.0 1.0
                    , TradeEvent    $ Trade    "456" Coinbase BTC 1.2 ETH   10.0 0.1 USD 1400.00
                    ])
                    [ Lot ETH 9.9 1400.0 (ExchangeLocation Coinbase)
                    , Lot BTC 0.3  360.6 (ExchangeLocation Coinbase)
                    ]

        describe "findLotSplits" $ do
            it "should handle an empty lot list" $
                shouldBe (findLotSplits (ExchangeLocation Coinbase) BTC 1.0 mempty) mempty

            it "should find a fully matching lot" $
                let ex        = ExchangeLocation Coinbase
                    lots      = [ Lot BTC 1.0 1000.0 ex ]
                    used      = lots
                    remaining = mempty
                in
                shouldBe (findLotSplits ex BTC 1.0 lots) (used, remaining)

            it "should find a partial matching lot" $
                let ex        = ExchangeLocation Coinbase
                    lots      = [ Lot BTC 1.0 1000.0 ex ]
                    used      = [ Lot BTC 0.8  800.0 ex ]
                    remaining = [ Lot BTC 0.2  200.0 ex ]
                in
                shouldBe (findLotSplits ex BTC 0.8 lots) (used, remaining)

            it "should find a split over multiple lots" $
                let ex        = ExchangeLocation Coinbase
                    lots      = [ Lot BTC 1.0 1000.0 ex, Lot BTC 0.5 600.0 ex, Lot BTC 2.0 3000.0 ex ]
                    used      = [ Lot BTC 1.0 1000.0 ex, Lot BTC 0.5 600.0 ex, Lot BTC 0.5  750.0 ex ]
                    remaining = [ Lot BTC 1.5 2250.0 ex]
                in
                shouldBe (findLotSplits ex BTC 2.0 lots) (used, remaining)

            it "should ignore lots in different locations" $
                let ex1       = ExchangeLocation Coinbase
                    ex2       = ExchangeLocation Gemini
                    lots      = [ Lot BTC 1.0 1000.0 ex1, Lot BTC 0.5 600.0 ex2 ]
                    used      = [ Lot BTC 1.0 1000.0 ex1 ]
                    remaining = [ Lot BTC 0.5  600.0 ex2 ]
                in
                shouldBe (findLotSplits ex1 BTC 2.0 lots) (used, remaining)
