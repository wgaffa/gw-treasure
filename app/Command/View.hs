module Command.View where

import System.Console.ANSI
import System.Console.Terminal.Size

import Data.Time

import Data.ByteString.Lazy.UTF8 (fromString)

import qualified Data.Vector as Vector
import qualified Data.Map as Map

import Helper.Terminal
import Internal.Transformations
import Internal.Presentation

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
        resets = resetTimes <$> logs
        missingLocs = missingLocations <$> logs

    header "Next Reset for characters"
    let resetStr = either (: []) (map (present timeZone) . Vector.toList) resets
    mapM_ putStrLn resetStr

    separator
    header "Spots not visited for each character"
    let missingStr = either (: []) (map presentMissing . Map.toList) missingLocs
    mapM_ putStrLn missingStr
    setSGR [Reset]
