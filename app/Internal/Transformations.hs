module Internal.Transformations where

import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Data.List

import Treasure

resetTimes :: Vector.Vector TreasureLog -> Vector.Vector TreasureLog
resetTimes = Vector.map reset
    where
        reset log@(TreasureLog _ time _) = log {tlLastAccessed = resetsAt time}

missingLocations :: Vector.Vector TreasureLog -> Map.Map PlayerName [Location]
missingLocations = Map.map unVisited . createMap
    where
        createMap = Map.fromListWith (++) . Vector.toList . mapByName
        mapByName = Vector.map (\ tl@(TreasureLog n _ l) -> (n, [l]))
        allLocations = [minBound .. maxBound] :: [Location]
        unVisited locs = allLocations \\ locs

toPlayerMap :: Vector.Vector TreasureLog -> Map.Map PlayerName [TreasureLog]
toPlayerMap = Map.fromListWith (++) . 
    Vector.toList . Vector.map (\ tl@(TreasureLog n _ _) -> (n, [tl]))