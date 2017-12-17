module Main ( main ) where

import qualified Data.Test

-- import           Options.Generic
--
-- data Command = Command { read :: Bool, file :: Maybe String }
--   deriving (Generic, Show)
--
-- instance ParseRecord Command
--
-- main :: IO ()
-- main = getRecord "BIP39" >>= \case

main :: IO ()
main = Data.Test.main
