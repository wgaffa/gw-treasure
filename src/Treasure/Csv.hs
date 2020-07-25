{-# LANGUAGE OverloadedStrings #-}

module Treasure.Csv where

import Data.Time
import Data.Time.Format

import Data.Csv
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Text.Read (readMaybe)

import Data.Vector (Vector(), toList)

import Treasure.Model

instance FromField Location where
    parseField = maybe (fail "Invalid location") pure . parseLocation . decodeUtf8

instance FromField PlayerName where
    parseField = maybe (fail "Invalid player name") pure . createPlayerName . decodeUtf8

instance FromField UTCTime where
    parseField = maybe (fail "Date time format invalid") pure . parseTimeM True defaultTimeLocale "%s" . BC.unpack

instance FromRecord TreasureLog where
    parseRecord v
        | length v == 3 = TreasureLog <$> v .! 0 <*> v .! 1 <*> v .! 2
        | otherwise = fail "Wrong number of fields"

instance ToField PlayerName where
    toField = encodeUtf8 . unPlayerName

instance ToField Location where
    toField = BC.pack . show

instance ToField UTCTime where
    toField = BC.pack . formatTime defaultTimeLocale "%s"

instance ToRecord TreasureLog where
    toRecord (TreasureLog name' time' location') =
        record [toField name', toField time', toField location']

readCsv :: ByteString -> Either String (Vector TreasureLog)
readCsv = decode NoHeader