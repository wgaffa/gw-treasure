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

presentLocation :: TimeZone -> LocationLog -> String
presentLocation timeZone (LocationLog (location, time)) =
    formatToString treasureLogFormat timeString location
    where
        timeFormat = formatTime defaultTimeLocale "%F %R" . utcToLocalTime timeZone
        timeString = maybe "Not recorded" timeFormat time

treasureLogFormat :: Format r (String -> Location -> r)
treasureLogFormat = "  " % right 25 ' ' % shown

playerLocationFormat :: Format r (PlayerName -> String -> r)
playerLocationFormat = right 25 ' ' % string