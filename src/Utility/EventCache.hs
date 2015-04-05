{-

    srem --- Timed reminders as notifications

    Copyright (C) 2015  Sean Whitton

    This file is part of srem.

    srem is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    srem is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with srem.  If not, see <http://www.gnu.org/licenses/>.

-}

module Utility.EventCache ( purgeOldEventCaches
                          , appendManualEventCache
                          , refreshEmacsEventCache
                          , readEmacsEventCache
                          , readManualEventCache
                          ) where

import           Control.Applicative ((<$>))
import           Control.Exception   (IOException, catch)
import           Control.Monad       (filterM, forM_, when)
import qualified Control.SremConfig  as SremConfig
import           Data.List.Split     (splitOn, splitOneOf)
import           Data.Maybe.Read
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           System.Directory    (createDirectoryIfMissing, doesFileExist,
                                      getDirectoryContents, removeFile)
import           System.FilePath     ((</>))
import           Types.Reminder
import           Utility.Emacs

-- TODO: lockfiles?

-- #### User interface functions

purgeOldEventCaches :: IO ()
purgeOldEventCaches = do
    dir <- SremConfig.getCacheDirectory
    files <- getDirectoryContents dir
             `catch` ((\_ -> return []) :: IOException -> IO [FilePath])
    today <- localDay . zonedTimeToLocalTime <$> getZonedTime
    forM_ files $ \file ->
         when (fileIsOldCache today file) $ removeFile (dir </> file)

appendManualEventCache   :: Reminder -> Day -> IO ()
appendManualEventCache r d = do
    dir <- SremConfig.getCacheDirectory
    createDirectoryIfMissing True dir
    let path = dir </> "manual_" ++ (showGregorian d) ++ ".csv"
    appendFile path $ makeEventsCSV [r]

refreshEmacsEventCache :: IO ()
refreshEmacsEventCache = do
    date <- todaysCacheFileDateString
    dir <- SremConfig.getCacheDirectory
    let path = dir </> "emacs_" ++ date ++ ".csv"
    removeFile path
    `catch` ((\_ -> return ()) :: IOException -> IO ())

readEmacsEventCache :: IO [Reminder]
readEmacsEventCache = do
    date <- todaysCacheFileDateString
    dir <- SremConfig.getCacheDirectory
    createDirectoryIfMissing True dir
    let path = dir </> "emacs_" ++ date ++ ".csv"
    doesFileExist path >>= \alreadyThere ->
        if   alreadyThere
        then readEventsCSV path
        else do
            rems <- parseEmacsOutput <$> getEmacsOutput
            writeFile path $ makeEventsCSV rems
            return rems

readManualEventCache :: IO [Reminder]
readManualEventCache = do
    date <- todaysCacheFileDateString
    dir <- SremConfig.getCacheDirectory
    let path = dir </> "manual_" ++ date ++ ".csv"
    doesFileExist path >>= \alreadyThere ->
        if   alreadyThere
        then readEventsCSV path
        else return []

-- #### Internal functions

readEventsCSV   :: FilePath -> IO [Reminder]
readEventsCSV f = readFile f >>= return . parseEventsCSV

parseEventsCSV :: String -> [Reminder]
parseEventsCSV = foldr step [] . lines
  where
    step r rs = maybe rs (: rs) $ parseEventCSV r

parseEventCSV      :: String -> Maybe Reminder
parseEventCSV line = do
    hourString:minuteString:text:[] <- return $ splitOn "," line
    hour <- readMaybe hourString
    minute <- readMaybe minuteString
    makeReminder' hour minute text

makeEventsCSV :: [Reminder] -> String
makeEventsCSV = unlines . foldr ((:) . makeEventCSV) []

makeEventCSV   :: Reminder -> String
makeEventCSV r = (show . getReminderHour $ r) ++ "," ++
                 (show . getReminderMinute $ r) ++ ","
                 ++ getReminderText r

todaysCacheFileDateString :: IO String
todaysCacheFileDateString = showGregorian . localDay . zonedTimeToLocalTime
                            <$> getZonedTime

fileIsOldCache            :: Day -> FilePath -> Bool
fileIsOldCache today file = length splitFile == 3
                            && splitFile !! 0 `elem` ["manual", "emacs"]
                            && (maybe False (< today) $ readDay $ splitFile !! 1)
                            && splitFile !! 2 == "csv"
  where
    splitFile             = splitOneOf "_." file

readDay   :: String -> Maybe Day
readDay s = do
    yearString:monthString:dayString:[] <- return $ splitOn "-" s
    year  <- readMaybe yearString
    month <- readMaybe monthString
    day   <- readMaybe dayString
    return $ fromGregorian year month day
