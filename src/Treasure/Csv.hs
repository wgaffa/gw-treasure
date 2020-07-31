{-# LANGUAGE OverloadedStrings #-}

module Treasure.Csv(
    readCsv
    , saveCsv
) where

import Data.Time

import Data.Csv
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text as Text
import Text.Read (readMaybe)

import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Data.List
import Data.Maybe (fromJust)

import Treasure.Model

-- | Data structure of a flat log file such as CSV files
type TreasureLog = (PlayerName, UTCTime, Location)

instance FromField Location where
    parseField = maybe (fail "Invalid location") pure . parseLocation . decodeUtf8

instance FromField PlayerName where
    parseField = maybe (fail "Invalid player name") pure . createPlayerName . decodeUtf8

instance FromField UTCTime where
    parseField = maybe (fail "Date time format invalid") pure . parseTimeM True defaultTimeLocale "%s" . BC.unpack

-- | Read a CSV string and remove any duplicate entries
readCsv :: ByteString -> Either String PlayerData
readCsv csv = do
    table <- decode NoHeader csv :: Either String (Vector.Vector TreasureLog)
    let removeDup = Vector.fromList . nubBy compare . Vector.toList
        compare (an, _, al) (bn, _, bl) = an == bn && al == bl
        playerLogMap = Map.fromListWith (Vector.++) . Vector.toList . Vector.map createPlayerLog
        in return . playerLogMap . removeDup $ table

saveCsv :: PlayerData -> ByteString
saveCsv = encode . csvList

createPlayerLog :: TreasureLog -> (PlayerName, Vector.Vector Treasure)
createPlayerLog (name, time, loc) =
    (name, Vector.singleton $ Treasure loc (Just time))

csvList :: PlayerData -> [(String, Int, String)]
csvList = map toPureCsvPrimitives . concatMap createCsv . Map.toAscList

createCsv :: (PlayerName, Vector.Vector Treasure) -> [(PlayerName, UTCTime, Location)]
createCsv (name, logs) = Vector.toList . Vector.mapMaybe toTuple $ logs
    where
        toTuple (Treasure loc (Just time)) = Just (name, time, loc)
        toTuple _ = Nothing

toPureCsvPrimitives :: (PlayerName, UTCTime, Location) -> (String, Int, String)
toPureCsvPrimitives (name, time, location) =
    let timeStamp = read $ formatTime defaultTimeLocale "%s" time
        player = Text.unpack . unPlayerName $ name
        locationName = show location
        in (player, timeStamp, locationName)