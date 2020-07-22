{-# LANGUAGE OverloadedStrings #-}

module Treasure (
    createPlayerName,
    parseLocation,
    resetsAt,
    TreasureLog (..),
    Location (..),
    getPlayerName,
    ) where

import Data.Time
import qualified Data.Text as T
import Data.Text.Encoding
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad (mzero)
import Control.Applicative (empty)
import Data.Monoid
import Data.Maybe
import Data.Csv
import Text.Read (readMaybe)

data Location = IssnurIsles | MehtaniKeys | ArkjokWard | BahdokCaverns |
    JahaiBluffs | MirrorOfLyss | ForumHighlands | HiddenCityOfAhdashim |
    RupturedHeart | SulfurousWastes | DomainOfPain | NightfallenJahai
    deriving (Eq, Enum, Bounded)

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

instance FromField Location where
    parseField = maybe (fail "Invalid location") pure . parseLocation . decodeUtf8

instance FromField PlayerName where
    parseField = maybe (fail "Invalid player name") pure . createPlayerName . BC.unpack

instance FromField UTCTime where
    parseField = maybe (fail "Date time format invalid") pure . readMaybe . BC.unpack

instance FromRecord TreasureLog where
    parseRecord v
        | length v == 3 = TreasureLog <$> v .! 0 <*> v .! 1 <*> v .! 2
        | otherwise = fail "Wrong number of fields"

instance ToField PlayerName where
    toField = BC.pack . getPlayerName

instance ToField Location where
    toField = BC.pack . show

instance ToField UTCTime where
    toField = BC.pack . show

instance ToRecord TreasureLog where
    toRecord (TreasureLog name' time' location') =
        record [toField name', toField time', toField location']

newtype PlayerName = PlayerName { getPlayerName :: String }
    deriving (Ord, Eq, Read, Show)

data TreasureLog = TreasureLog {
    playerName   :: PlayerName,
    lastAccessed :: UTCTime,
    location     :: Location
} deriving (Show, Read)

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

resetsAt :: TreasureLog -> UTCTime
resetsAt (TreasureLog _ time _) =
    let resetTime = addDays 30 $ utctDay time
    in UTCTime resetTime $ utctDayTime time