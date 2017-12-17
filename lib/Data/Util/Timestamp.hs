module Data.Util.Timestamp
    ( Timestamp(..)
    ) where


newtype Timestamp = Timestamp Int
  deriving (Enum, Eq, Ord)
