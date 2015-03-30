module EventCache ( purgeOldEventCaches
                  , appendManualEventCache
                  , readEmacsEventCache
                  , readManualEventCache
                  ) where

import           Control.Applicative ((<$>))
import           Control.Exception   (IOException, catch)
import           Control.Monad       (filterM, forM_, when)
import qualified Control.SremConfig  as SremConfig
import           Data.List.Split     (splitOneOf)
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
    today <- todaysCacheFileDateString
    forM_ files $ \file ->
        when (fileIsOldCache file) $ removeFile file

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

readEventsCSV :: FilePath -> IO [Reminder]
readEventsCSV = undefined

parseEventsCSV :: String -> [Reminder]
parseEventsCSV = undefined

makeEventsCSV :: [Reminder] -> String
makeEventsCSV = undefined

todaysCacheFileDateString :: IO String
todaysCacheFileDateString = showGregorian . utctDay <$> getCurrentTime

fileIsOldCache :: FilePath -> Bool
fileIsOldCache = undefined

-- getEventCacheLock :: IO [Reminder]
-- getEventCacheLock = undefined
