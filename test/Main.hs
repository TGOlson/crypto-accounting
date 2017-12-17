module Main (main) where

import qualified Test.Hspec

import qualified Data.ExchangeSpec

main :: IO ()
main = mapM_ Test.Hspec.hspec specs

specs :: [Test.Hspec.Spec]
specs = [ Data.ExchangeSpec.spec
        ]
