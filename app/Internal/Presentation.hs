{-# LANGUAGE OverloadedStrings #-}
module Internal.Presentation where

import qualified Data.Text.Lazy.Builder as LTB
import Formatting
import Formatting.Buildable

import Data.Time
import Data.List

import Treasure

instance Buildable PlayerName where
    build = LTB.fromText . unPlayerName

presentMissing :: (PlayerName, [Location]) -> String
presentMissing (name, locs) =
    formatToString playerLocationFormat name (intercalate ", " (map show locs))

presentLog :: TimeZone -> TreasureLog -> String
presentLog timeZone (TreasureLog _ time location) =
    formatToString treasureLogFormat timeFormat location
    where
        localTime = utcToLocalTime timeZone
        timeFormat = formatTime defaultTimeLocale "%F %R" $ localTime time

treasureLogFormat :: Format r (String -> Location -> r)
treasureLogFormat = "  " % right 25 ' ' % shown

playerLocationFormat :: Format r (PlayerName -> String -> r)
playerLocationFormat = right 25 ' ' % string