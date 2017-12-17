{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.CSV.Instances () where

import Data.Csv
import Data.Scientific

instance FromField Rational where
    parseField bs = toRational <$> (parseField bs :: Parser Scientific)
