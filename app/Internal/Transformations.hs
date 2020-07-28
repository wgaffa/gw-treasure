module Internal.Transformations where

import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Data.List

import qualified Data.Text as T

import Treasure

resetTimes :: Map.Map PlayerName (Vector.Vector LocationLog) -> Map.Map PlayerName (Vector.Vector LocationLog)
resetTimes = Map.map reset
    where
        reset = Vector.map resetLocation
        resetLocation (LocationLog (loc, time)) = LocationLog (loc, resetsAt <$> time)
