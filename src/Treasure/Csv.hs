{-# LANGUAGE OverloadedStrings #-}

module Treasure.Csv(
    readCsv
) where

import Data.Time

import Data.Csv
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Text.Read (readMaybe)

import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Data.List

import Treasure.Model

-- | Data structure of a flat log file such as CSV files
data TreasureLog = TreasureLog {
    tlPlayerName   :: PlayerName,
    tlLastAccessed :: UTCTime,
    tlLocation     :: Location
} deriving (Show)

instance Eq TreasureLog where
    (==) a b = tlPlayerName a == tlPlayerName b
        && tlLocation a == tlLocation b

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

-- | Read a CSV string and remove any duplicate entries
readCsv :: ByteString -> Either String (Map.Map PlayerName (Vector.Vector LocationLog))
readCsv csv = do
    table <- decode NoHeader csv :: Either String (Vector.Vector TreasureLog)
    let removeDup = Vector.fromList . nub . Vector.toList
        playerLogMap = Map.fromListWith (Vector.++) . Vector.toList . Vector.map createPlayerLog
        in return . playerLogMap . removeDup $ table

createPlayerLog :: TreasureLog -> PlayerLog
createPlayerLog (TreasureLog name time loc) = (name, Vector.singleton $ LocationLog (loc, Just time)) 
