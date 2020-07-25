{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.Vector as Vector
import Data.List (unlines)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB

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
    
    putStr $ either id (present timeZone) resets

resetTimes :: Vector.Vector TreasureLog -> Vector.Vector TreasureLog
resetTimes = Vector.map reset
    where
        reset (TreasureLog name time loc) = TreasureLog name (resetsAt time) loc

present :: TimeZone -> Vector.Vector TreasureLog -> String
present timeZone = vectorUnlines . Vector.map presentLog
    where 
        vectorUnlines = Vector.foldr (\ x acc -> x ++ "\r\n" ++ acc) ""
        presentLog (TreasureLog name time location) = 
            formatToString (right 25 ' ' % right 25 ' ' % shown)
            name (timeFormat $ localTime time) location
        localTime = utcToLocalTime timeZone
        timeFormat = formatTime defaultTimeLocale "%F %R"