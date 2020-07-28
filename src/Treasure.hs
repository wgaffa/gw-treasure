{-# LANGUAGE OverloadedStrings #-}

module Treasure (
    Location (..)
    , LocationLog(..)
    , PlayerName()
    , PlayerLog
    , createPlayerName
    , unPlayerName
    , parseLocation
    , resetsAt
    , addMissingLocations
    ) where

import qualified Data.Vector as Vector
import Data.List

import Data.Time
import Treasure.Model

resetsAt :: UTCTime -> UTCTime
resetsAt time =
    let resetTime = addDays 30 $ utctDay time
    in UTCTime resetTime $ utctDayTime time

addMissingLocations :: Vector.Vector LocationLog -> Vector.Vector LocationLog
addMissingLocations locations = locations Vector.++ transform locations
    where
        transform = Vector.fromList . map missingLoc . missingLocations
        missingLocations logs = [minBound .. maxBound] \\ visited logs
        visited = Vector.toList . Vector.map (fst . unLocationLog)
        missingLoc loc = LocationLog (loc, Nothing)