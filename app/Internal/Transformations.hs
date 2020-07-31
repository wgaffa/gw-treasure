module Internal.Transformations where

import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Data.List

import qualified Data.Text as T

import Treasure

resetTimes :: PlayerData -> PlayerData
resetTimes = Map.map reset . Map.map addMissingLocations
    where
        reset = Vector.map resetLocation
        resetLocation (Treasure loc time) = Treasure loc $ resetsAt <$> time
