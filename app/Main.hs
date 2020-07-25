{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Console.ANSI
import System.Console.Terminal.Size
import System.Environment
import GHC.IO.Encoding

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

import Treasure
import Treasure.Csv

instance Buildable PlayerName where
    build = LTB.fromText . unPlayerName

main :: IO ()
main = do
    setLocaleEncoding utf8
    timeZone <- getCurrentTimeZone
    (file:args) <- getArgs
    contents <- readFile file
    let logs = readCsv $ fromString contents
        resets = resetTimes <$> logs
        missingLocs = missingLocations <$> logs

    header "Next Reset for characters"
    let resetStr = either (: []) (map (present timeZone) . Vector.toList) resets
    mapM_ putStrLn resetStr

    separator
    header "Spots not visited for each character"
    let missingStr = either (: []) (map presentMissing . M.toList) missingLocs
    mapM_ putStrLn missingStr
    setSGR [Reset]

separator :: IO ()
separator = do
    windowSize <- size
    let terminalWidth = maybe 0 width windowSize
    putStrLn $ replicate terminalWidth '\x2500'

header :: String -> IO ()
header str = do
    setSGR [SetColor Foreground Dull Green]
    putStrLn str
    setSGR []

resetTimes :: Vector.Vector TreasureLog -> Vector.Vector TreasureLog
resetTimes = Vector.map reset
    where
        reset log@(TreasureLog _ time _) = log {tlLastAccessed = resetsAt time}

missingLocations :: Vector.Vector TreasureLog -> M.Map PlayerName [Location]
missingLocations = M.map unVisited . createMap
    where
        createMap = M.fromListWith (++) . Vector.toList . mapByName
        mapByName = Vector.map (\ tl@(TreasureLog n _ l) -> (n, [l]))
        allLocations = [minBound .. maxBound] :: [Location]
        unVisited locs = allLocations \\ locs

presentMissing :: (PlayerName, [Location]) -> String
presentMissing (name, locs) = 
    formatToString playerLocationFormat name (intercalate ", " (map show locs))

present :: TimeZone -> TreasureLog -> String
present timeZone (TreasureLog name time location) = 
    formatToString treasureLogFormat name timeFormat location
    where
        localTime = utcToLocalTime timeZone
        timeFormat = formatTime defaultTimeLocale "%F %R" $ localTime time

treasureLogFormat :: Format r (PlayerName -> String -> Location -> r)
treasureLogFormat = right 25 ' ' % right 25 ' ' % shown

playerLocationFormat :: Format r (PlayerName -> String -> r)
playerLocationFormat = right 25 ' ' % string