{-# LANGUAGE OverloadedStrings #-}

module Treasure.Model (
      Location(..)
    , TreasureLog(..)
    , PlayerName()
    , LocationLog(..)
    , PlayerLog
    , unPlayerName
    , createPlayerName
    , createPlayerLog
    , parseLocation
    ) where

import Data.Time
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Monoid (First(..), getFirst)

import Data.Vector (Vector, singleton)

data Location = IssnurIsles | MehtaniKeys | ArkjokWard | BahdokCaverns |
    JahaiBluffs | MirrorOfLyss | ForumHighlands | HiddenCityOfAhdashim |
    RupturedHeart | SulfurousWastes | DomainOfPain | NightfallenJahai
    deriving (Eq, Enum, Bounded)

newtype PlayerName = PlayerName { unPlayerName :: T.Text }
    deriving (Ord, Eq, Read, Show)

-- | Data structure of a flat log file such as CSV files
data TreasureLog = TreasureLog {
    tlPlayerName   :: PlayerName,
    tlLastAccessed :: UTCTime,
    tlLocation     :: Location
} deriving (Show)

-- | Denotes when a treasure was last accessed,
-- Nothing denotes that there is no record of it being opened.
-- A location log is equal to another if their location is equal.
newtype LocationLog = LocationLog { unLocationLog :: (Location, Maybe UTCTime) }
    deriving (Show)

-- | Structure for representing a players log
type PlayerLog = (PlayerName, Vector LocationLog)

instance Eq TreasureLog where
    (==) a b = tlPlayerName a == tlPlayerName b
        && tlLocation a == tlLocation b

instance Eq LocationLog where
    (==) a b = fst (unLocationLog a) == fst (unLocationLog b)

instance Show Location where
    show IssnurIsles = "Issnur Isles"
    show MehtaniKeys = "Mehtani Keys"
    show ArkjokWard = "Arkjok Ward"
    show BahdokCaverns = "Bahdok Caverns"
    show JahaiBluffs = "Jahai Bluffs"
    show MirrorOfLyss = "The Mirror of Lyss"
    show ForumHighlands = "Forum Highlands"
    show HiddenCityOfAhdashim = "The Hidden City of Ahdashim"
    show RupturedHeart = "The Ruptured Heart"
    show SulfurousWastes = "The Sulfurous Wastes"
    show DomainOfPain = "Domain of Pain"
    show NightfallenJahai = "Nightfallen Jahai"

instance Read Location where
    readsPrec _ loc = case parseLocation $ T.pack loc of
        Nothing -> []
        Just x -> [(x, "")]

createPlayerName :: T.Text -> Maybe PlayerName
createPlayerName name
    | T.length name >= 3 && wordCount > 1 = Just $ PlayerName name
    | otherwise = Nothing
    where wordCount = length . T.words $ name

createPlayerLog :: TreasureLog -> PlayerLog
createPlayerLog (TreasureLog name time loc) = (name, singleton $ LocationLog (loc, Just time)) 

parseLocation :: T.Text -> Maybe Location
parseLocation loc
    | T.null loc = Nothing
    | otherwise = firstMatch
    where checkPrefix name l =
            if T.isPrefixOf (lowerCase name) (lowerCase . T.pack $ show l)
                then Just l
                else Nothing
          lowerCase x = fromMaybe (T.strip . T.toLower $ x) $ stripLeading x
          stripLeading s = stripLeadingThe s >>= stripSpaces
          stripLeadingThe = T.stripPrefix "the" . T.toLower
          stripSpaces = Just . T.strip
          find = map (checkPrefix loc) allLocations
          allLocations = [minBound .. maxBound]
          firstMatch = getFirst . mconcat . map First $ find