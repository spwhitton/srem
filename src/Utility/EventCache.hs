module EventCache ( purgeOldEventCaches
                  , appendManualEventCache
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
import           Data.Time.Clock
import           System.Directory    (doesFileExist, getDirectoryContents,
                                      removeFile)
import           System.FilePath     ((</>))
import           Types.Reminder
import           Utility.Emacs

-- TODO: lockfiles?

-- #### User interface functions

purgeOldEventCaches :: IO ()
purgeOldEventCaches = do
    files <- (SremConfig.getCacheDirectory
             >>= getDirectoryContents)
             `catch` ((\_ -> return []) :: IOException -> IO [FilePath])
    today <- utctDay <$> getCurrentTime
    forM_ files $ \file ->
        when (fileIsOldCache today file) $ removeFile file

appendManualEventCache   :: Reminder -> Day -> IO ()
appendManualEventCache r d = do
    dir <- SremConfig.getCacheDirectory
    let path = dir </> "manual_" ++ (showGregorian d) ++ ".csv"
    appendFile path $ makeEventsCSV [r]

readEmacsEventCache :: IO [Reminder]
readEmacsEventCache = do
    date <- todaysCacheFileDateString
    dir <- SremConfig.getCacheDirectory
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
todaysCacheFileDateString = showGregorian . utctDay <$> getCurrentTime

fileIsOldCache            :: Day -> FilePath -> Bool
fileIsOldCache today file = length splitFile == 3
                            && splitFile !! 1 `elem` ["manual", "emacs"]
                            && (maybe False (< today) $ readDay $ splitFile !! 2)
                            && splitFile !! 3 == "csv"
  where
    splitFile             = splitOneOf "_." file

readDay   :: String -> Maybe Day
readDay s = do
    yearString:monthString:dayString:[] <- return $ splitOn "-" s
    year  <- readMaybe yearString
    month <- readMaybe monthString
    day   <- readMaybe dayString
    return $ fromGregorian year month day
