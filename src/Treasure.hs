{-# LANGUAGE OverloadedStrings #-}

module Treasure (
    TreasureLog (..)
    , Location (..)
    , createPlayerName
    , unPlayerName
    , parseLocation
    , resetsAt
    ) where

import Data.Time
import Treasure.Model

resetsAt :: TreasureLog -> UTCTime
resetsAt (TreasureLog _ time _) =
    let resetTime = addDays 30 $ utctDay time
    in UTCTime resetTime $ utctDayTime time