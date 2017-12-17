{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.CSV.Instances () where

import qualified Data.ByteString.Char8 as C8
import           Data.Csv
import           Data.Monoid
import           Data.Time
import           Safe

parseWithRead :: Read a => String -> C8.ByteString -> Parser a
parseWithRead tag bs = maybe err pure $ readMay str
  where
    str = C8.unpack bs
    err = fail $ "Unable to parse " <> tag <> " from '" <> str <> "'"

instance FromField UTCTime where
    parseField = parseWithRead "UTCTime"
