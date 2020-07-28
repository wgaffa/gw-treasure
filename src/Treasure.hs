{-# LANGUAGE OverloadedStrings #-}

module Treasure (
    TreasureLog (..)
    , Location (..)
    , LocationLog(..)
    , PlayerName()
    , PlayerLog
    , createPlayerName
    , createPlayerLog
    , unPlayerName
    , parseLocation
    , resetsAt
    ) where

import Data.Time
import Treasure.Model

resetsAt :: UTCTime -> UTCTime
resetsAt time =
    let resetTime = addDays 30 $ utctDay time
    in UTCTime resetTime $ utctDayTime time