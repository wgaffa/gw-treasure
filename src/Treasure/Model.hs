{-# LANGUAGE OverloadedStrings #-}

module Treasure.Model (
      Location(..)
    , TreasureLog(..)
    , PlayerName()
    , unPlayerName
    , createPlayerName
    , parseLocation
    ) where

import Data.Time
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Monoid (First(..), getFirst)

data Location = IssnurIsles | MehtaniKeys | ArkjokWard | BahdokCaverns |
    JahaiBluffs | MirrorOfLyss | ForumHighlands | HiddenCityOfAhdashim |
    RupturedHeart | SulfurousWastes | DomainOfPain | NightfallenJahai
    deriving (Eq, Enum, Bounded)

newtype PlayerName = PlayerName { unPlayerName :: String }
    deriving (Ord, Eq, Read, Show)

data TreasureLog = TreasureLog {
    playerName   :: PlayerName,
    lastAccessed :: UTCTime,
    location     :: Location
} deriving (Show, Read)

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

createPlayerName :: String -> Maybe PlayerName
createPlayerName name
    | length name >= 3 && wordCount > 1 = Just $ PlayerName name
    | otherwise = Nothing
    where wordCount = length . words $ name

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