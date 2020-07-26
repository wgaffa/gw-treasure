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

present :: TimeZone -> TreasureLog -> String
present timeZone (TreasureLog name time location) =
    formatToString treasureLogFormat name timeFormat location
    where
        localTime = utcToLocalTime timeZone
        timeFormat = formatTime defaultTimeLocale "%F %R" $ localTime time

treasureLogFormat :: Format r (PlayerName -> String -> Location -> r)
treasureLogFormat = right 25 ' ' % right 25 ' ' % shown

playerLocationFormat :: Format r (PlayerName -> String -> r)
playerLocationFormat = right 25 ' ' % string