module Data.Util.RationalF
    ( RationalF(..)
    ) where

import Data.Csv
import Data.Data
import Data.Scientific
import Numeric
-- import Foreign.Storable

-- Rational, but with some better formatting instances
-- TODO: Integral instance?
newtype RationalF = RationalF Rational
  deriving (Data, Enum, Eq, Fractional, Num, Ord, Read, Real, RealFrac)

instance FromField RationalF where
    parseField bs = (RationalF . toRational) <$> (parseField bs :: Parser Scientific)

instance Show RationalF where
    show (RationalF r) = showFFloatAlt Nothing (fromRational r :: Double) mempty
