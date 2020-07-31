{-# LANGUAGE OverloadedStrings #-}

module Treasure.Model (
      Location(..)
    , PlayerName()
    , Treasure(..)
    , PlayerData
    , Player
    , unPlayerName
    , createPlayerName
    , parseLocation
    ) where

import Data.Time
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Monoid (First(..), getFirst)

import Data.Vector (Vector, singleton)
import Data.Map (Map)

data Location = IssnurIsles | MehtaniKeys | ArkjokWard | BahdokCaverns |
    JahaiBluffs | MirrorOfLyss | ForumHighlands | HiddenCityOfAhdashim |
    RupturedHeart | SulfurousWastes | DomainOfPain | NightfallenJahai
    deriving (Eq, Enum, Bounded)

newtype PlayerName = PlayerName { unPlayerName :: T.Text }
    deriving (Ord, Eq, Read, Show)

data Treasure = Treasure {
    treasureLocation :: Location
    , treasureOpened :: Maybe UTCTime
} deriving (Show)

type Player = (PlayerName, Vector Treasure)
type PlayerData = Map PlayerName (Vector Treasure)

instance Eq Treasure where
    (==) a b = treasureLocation a == treasureLocation b

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