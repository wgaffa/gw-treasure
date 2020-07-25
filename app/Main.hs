{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe

import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.Vector as Vector
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB

import qualified Data.Map as M
import Data.List

import Data.Time

import Formatting
import Formatting.Buildable
import Formatting.Formatters

import Treasure
import Treasure.Csv

instance Buildable PlayerName where
    build = LTB.fromText . unPlayerName

main :: IO ()
main = do
    timeZone <- getCurrentTimeZone
    contents <- getContents
    let logs = readCsv $ fromString contents
        resets = resetTimes <$> logs
        missing = missingLocations <$> logs

    putStr $ either id (present timeZone) resets
    putStrLn "---"
    putStr $ either id presentMissing missing

resetTimes :: Vector.Vector TreasureLog -> Vector.Vector TreasureLog
resetTimes = Vector.map reset
    where
        reset (TreasureLog name time loc) = TreasureLog name (resetsAt time) loc

missingLocations :: Vector.Vector TreasureLog -> M.Map PlayerName [Location]
missingLocations = M.map unVisited . createMap
    where
        createMap = M.fromListWith (++) . Vector.toList . mapByName
        mapByName = Vector.map (\ tl@(TreasureLog n _ l) -> (n, [l]))
        allLocations = [minBound .. maxBound] :: [Location]
        unVisited locs = allLocations \\ locs

presentMissing :: M.Map PlayerName [Location] -> String
presentMissing = unlines . map missingString . M.toList
    where
        missingString (name, locs) = playerName name ++ ": " ++ intercalate ", " (locationTranslate locs)
        locationTranslate = map show
        playerName = T.unpack . unPlayerName

present :: TimeZone -> Vector.Vector TreasureLog -> String
present timeZone = vectorUnlines . Vector.map presentLog
    where
        vectorUnlines = Vector.foldr (\ x acc -> x ++ "\r\n" ++ acc) ""
        presentLog (TreasureLog name time location) =
            formatToString (right 25 ' ' % right 25 ' ' % shown)
            name (timeFormat $ localTime time) location
        localTime = utcToLocalTime timeZone
        timeFormat = formatTime defaultTimeLocale "%F %R"