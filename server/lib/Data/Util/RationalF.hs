module Data.Util.RationalF
    ( RationalF(..)
    ) where

import           Data.Aeson
import           Data.Csv
import           Data.Data
import           Data.Scientific
import qualified Data.Text       as T
import           Numeric

-- Rational, but with some better formatting instances
-- TODO: Integral instance?
newtype RationalF = RationalF Rational
  deriving (Data, Enum, Eq, Fractional, Num, Ord, Read, Real, RealFrac)

fromReal :: Real a => a -> RationalF
fromReal = RationalF . toRational

fromScientific :: Scientific -> RationalF
fromScientific = fromReal

instance FromField RationalF where
    parseField = fmap fromScientific . parseField

instance Show RationalF where
    show (RationalF r) = showFFloatAlt Nothing (fromRational r :: Double) mempty

instance ToJSON RationalF where
    toJSON = String . T.pack . show

instance FromJSON RationalF where
    parseJSON = fmap fromScientific . parseJSON
