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

    header "Next Reset for characters"
    let resets = resetTimes <$> logs
        resetStr = either (: []) (map (presentLog timeZone) . mapping) resets
        mapping = concat . Map.elems . toPlayerMap
        in mapM_ putStrLn resetStr

    separator
    header "Spots not visited for each character"
    let missingLocs = missingLocations <$> logs
        missingStr = either (: []) (map presentMissing . Map.toList) missingLocs
        in mapM_ putStrLn missingStr
    setSGR [Reset]
