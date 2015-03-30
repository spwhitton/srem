module EventCache ( purgeOldEventCaches
                  , appendManualEventCache
                  , readEmacsEventCache
                  , readManualEventCache
                  ) where

import           Control.Applicative ((<$>))
import qualified Control.SremConfig  as SremConfig
import           Data.Time.Calendar
import           Data.Time.Clock
import           System.Directory    (doesFileExist)
import           System.FilePath     ((</>))
import           Types.Reminder
import           Utility.Emacs

purgeOldEventCaches :: IO [Reminder]
purgeOldEventCaches = undefined

appendManualEventCache :: IO ()
appendManualEventCache = do
    date <- cacheFileDateString
    dir <- SremConfig.getCacheDirectory
    let path = dir </> "manual_" ++ date ++ ".csv"
    undefined

readEmacsEventCache :: IO [Reminder]
readEmacsEventCache = do
    date <- cacheFileDateString
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
    date <- cacheFileDateString
    dir <- SremConfig.getCacheDirectory
    let path = dir </> "manual_" ++ date ++ ".csv"
    undefined

readEventsCSV :: FilePath -> IO [Reminder]
readEventsCSV = undefined

parseEventsCSV :: String -> [Reminder]
parseEventsCSV = undefined

makeEventsCSV :: [Reminder] -> String
makeEventsCSV = undefined

cacheFileDateString :: IO String
cacheFileDateString = showGregorian . utctDay <$> getCurrentTime

-- getEventCacheLock :: IO [Reminder]
-- getEventCacheLock = undefined
