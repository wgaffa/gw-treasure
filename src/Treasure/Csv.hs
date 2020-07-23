{-# LANGUAGE OverloadedStrings #-}

module Treasure.Csv where

import Data.Time

import Data.Csv
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Text.Read (readMaybe)

import Treasure.Model

instance FromField Location where
    parseField = maybe (fail "Invalid location") pure . parseLocation . decodeUtf8

instance FromField PlayerName where
    parseField = maybe (fail "Invalid player name") pure . createPlayerName . BC.unpack

instance FromField UTCTime where
    parseField = maybe (fail "Date time format invalid") pure . readMaybe . BC.unpack

instance FromRecord TreasureLog where
    parseRecord v
        | length v == 3 = TreasureLog <$> v .! 0 <*> v .! 1 <*> v .! 2
        | otherwise = fail "Wrong number of fields"

instance ToField PlayerName where
    toField = BC.pack . unPlayerName

instance ToField Location where
    toField = BC.pack . show

instance ToField UTCTime where
    toField = BC.pack . show

instance ToRecord TreasureLog where
    toRecord (TreasureLog name' time' location') =
        record [toField name', toField time', toField location']