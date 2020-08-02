module Command.Update(
    update
) where

import qualified Data.ByteString.Lazy.Char8 as BLC
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
        Right playerData -> print $ updatePlayer playerData player location now
    return ()
update args = usage args

updatePlayer :: PlayerData -> String -> String -> UTCTime -> Maybe PlayerData
updatePlayer table name location time = do
    player <- findPlayer table name
    playerName <- createPlayerName $ Text.pack name
    treasure <- findLocation player location
    let timeStamp = updateTimeStamp treasure time
    index <- Vector.elemIndex treasure player
    let newTreasures = player Vector.// [(index, timeStamp)]
    return $ Map.adjust (const newTreasures) playerName table

updateTimeStamp :: Treasure -> UTCTime -> Treasure
updateTimeStamp treasure time = treasure {treasureOpened = Just time}

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
