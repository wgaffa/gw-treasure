module Command.View where

import System.Console.ANSI
import System.Console.Terminal.Size

import Data.Time

import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.Text as T

import qualified Data.Vector as Vector
import qualified Data.Map as Map

import Helper.Terminal
import Internal.Transformations
import Internal.Presentation

import Treasure
import Treasure.Csv

view :: [String] -> IO ()
view [] = do
    contents <- getContents
    viewReset contents
view [filename] = do
    contents <- readFile filename
    viewReset contents

viewReset :: String -> IO ()
viewReset contents = do
    timeZone <- getCurrentTimeZone
    let logs = readCsv $ fromString contents

    header "Next Reset for characters"
    let resets = resetTimes <$> logs
        players = toPlayerMap <$> resets
        in mapM_ playerReset $ either (const []) Map.toList players

    separator
    header "Spots not visited for each character"
    let missingLocs = missingLocations <$> logs
        missingStr = either (: []) (map presentMissing . Map.toList) missingLocs
        in mapM_ putStrLn missingStr
    setSGR [Reset]

playerReset :: PlayerLog -> IO ()
playerReset (name, logs) = do
    putStrLn . T.unpack . unPlayerName $ name
    timeZone <- getCurrentTimeZone
    mapM_ (putStrLn . presentLocation timeZone) logs