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
    interact (present timeZone)

present :: TimeZone -> String -> String
present timeZone = either id prettyPrint . readCsv . fromString
    where 
        prettyPrint = unlines . map presentLog . Vector.toList
        presentLog (TreasureLog name time location) = 
            formatToString (right 25 ' ' % right 25 ' ' % shown)
            name (timeFormat $ localTime time) location
        localTime = utcToLocalTime timeZone
        timeFormat = formatTime defaultTimeLocale "%F %R"