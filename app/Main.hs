{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.Vector as Vector
import Data.List (unlines)
import qualified Data.Text.Lazy as LT

import Data.Time

import Formatting
import Formatting.Formatters

import Treasure
import Treasure.Csv

main :: IO ()
main = do
    contents <- getContents
    putStr (present contents)

present :: String -> String
present = either id prettyPrint . readCsv . fromString

prettyPrint :: Vector.Vector TreasureLog -> String
prettyPrint = unlines . map presentLog . Vector.toList

presentLog :: TreasureLog -> String
presentLog (TreasureLog name time location) =
    formatToString (right 25 ' ' % right 45 ' ' % shown)
        (unPlayerName name) time location
