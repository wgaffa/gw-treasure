module Command.Update(
    update
) where

import System.IO
import System.Directory
import GHC.IO.Handle

import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.Map ((!?))
import qualified Data.Vector as Vector

import Data.Time

import Command.Args

import Treasure.Csv
import Treasure.Model

update :: [String] -> IO ()
update ["--", player, location] = do
    contents <- getContents
    now <- getCurrentTime
    let csv = readCsv $ BLC.pack contents
    case csv of
        Left msg -> putStrLn msg
        Right playerData -> let record = updatePlayer playerData player location now
                                in case record of
                                    Just x -> putStrLn $ BLC.unpack (saveCsv x)
                                    Nothing -> putStrLn "Unable to update record"
update [filename, player, location] = do
    contents <- readFile filename
    (tempName, handle) <- openTempFile "." "temp"
    now <- getCurrentTime
    let csv = readCsv $ BLC.pack contents
    case csv of
        Left msg -> putStrLn msg
        Right playerData -> let record = updatePlayer playerData player location now
                                in case record of
                                    Just x -> hPutStr handle $ BLC.unpack (saveCsv x)
                                    Nothing -> putStrLn "Unable to update record"
    hClose handle
    removeFile filename
    renameFile tempName filename
update args = usage args

slurp :: Handle -> IO String
slurp handle = do
    h <- hDuplicate handle
    hSeek h AbsoluteSeek 0
    hGetContents h

updatePlayer :: PlayerData -> String -> String -> UTCTime -> Maybe PlayerData
updatePlayer table name location time = do
    player <- findPlayer table name
    playerName <- createPlayerName $ Text.pack name
    treasure <- findLocation player location
    let timeStamp = updateTimeStamp treasure time
    -- index <- Vector.elemIndex treasure player
    -- let newTreasures = player Vector.// [(index, timeStamp)]
    updateFunc <- flip <$> updateLocation player location <*> return timeStamp
    return $ Map.adjust updateFunc playerName table

updateTimeStamp :: Treasure -> UTCTime -> Treasure
updateTimeStamp treasure time = treasure {treasureOpened = Just time}

updateLocation :: Vector.Vector Treasure -> String -> Maybe (Vector.Vector Treasure -> Treasure -> Vector.Vector Treasure)
updateLocation treasures locationStr = do
    treasure <- findLocation treasures locationStr
    case treasureOpened treasure of
        Just time -> do
            index <- Vector.elemIndex treasure treasures
            return (\p t -> p Vector.// [(index, t)])
        Nothing -> return Vector.snoc

findPlayer :: PlayerData -> String -> Maybe (Vector.Vector Treasure)
findPlayer playerData name = do
    player <- createPlayerName $ Text.pack name
    playerData !? player

findLocation :: Vector.Vector Treasure -> String -> Maybe Treasure
findLocation treasures locationStr = do
    location <- parseLocation $ Text.pack locationStr
    let compareLocation (Treasure l _) = l == location
    case Vector.find compareLocation treasures of
        Just treasure -> return treasure
        Nothing -> return $ Treasure location Nothing
