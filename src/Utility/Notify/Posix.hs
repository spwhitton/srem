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

module Utility.Notify.Posix (sendNotifications) where

import           Control.Applicative (pure, (<$>), (<*>))
import           Control.Exception   (try)
import           Control.Monad       (filterM, foldM, liftM, mapM)
import           Data.Function       (on)
import           Data.List           (sortBy)
import           Data.Time.Clock     (UTCTime)
import           System.Directory    (doesDirectoryExist, getDirectoryContents,
                                      getHomeDirectory, getModificationTime)
import           System.Environment  (getEnvironment, setEnv)
import           System.FilePath     ((</>))
import           System.Process      (readProcessWithExitCode)

import           DBus                (Address, parseAddress)
import           DBus.Client         (Client, ClientError, connect)
import           DBus.Notify
import           DBus.Socket         (SocketError)

import qualified Control.SremConfig  as SremConfig
import           Data.List.Split     (splitOn)
import           Types.Reminder

sendNotifications   :: [Reminder] -> IO ()
sendNotifications rems =
    getDBusUserAddress >>=
    maybe (return ()) (\address -> do
                            maybeClient <- (try $ connect address) :: IO (Either SocketError Client)
                            either (\_ -> return ()) (\client -> mapM_ (sendNotification client) rems) maybeClient)

sendNotification       :: Client -> Reminder -> IO ()
sendNotification c rem = do
    let (title:content) = words . getReminderText $ rem
    soundFile <- SremConfig.notificationSound
    notify c blankNote { appName = "srem"
                                            , body    = (Just . Text . unwords) content
                                            , summary = title
                                            , expiry  = Milliseconds 10000
                                            , hints   = [ SoundFile soundFile ]}
    (_, _, _) <- readProcessWithExitCode "aplay" [soundFile] ""
    return ()

getDBusUserAddress :: IO (Maybe Address)
getDBusUserAddress = do
    -- TODO: catch various IO exceptions!
    dir <- (</>) <$> getHomeDirectory <*> pure (".dbus" </> "session-bus")
    socketFile <- getNewestRealFile dir
    -- let realContents = filter (`notElem` [".", ".."]) contents
    -- socketFile <- return realContents
    --               >>= mapM (return . ((dir ++ "/")  ++))
    --               >>= newestFile
    addr <- dropWhile (/= 'u') . head . filter findSocketLine . lines
            <$> readFile socketFile
    return (return addr >>= parseAddress)
  where
    findSocketLine line = takeWhile (/= '=') line == "DBUS_SESSION_BUS_ADDRESS"

-- | Return the full path to the newest file in a directory, or the
-- empty string if passed a file or empty directory or a path that
-- doesn't exist.
getNewestRealFile     :: FilePath -> IO FilePath
getNewestRealFile dir = do
    doesDirectoryExist dir >>= \exists ->
        if exists then do
            contents <- map (dir </>). filter (`notElem` [".", ".."])
                        <$> getDirectoryContents dir
            contentsWithModTimes <- foldM step [] contents
            let sorted = sortBy
                         (flip $ (compare `on` snd))
                         contentsWithModTimes
            if null sorted
                then return ""
                else return . fst . head $ sorted
        else return ""
  where
    step pairs file = do
        fileModTime <- getModificationTime file
        return $ (file, fileModTime) : pairs

-- newestFile    :: [FilePath] -> IO FilePath
-- newestFile xs = do
--     modTimes <- getModTimes xs
--     let sorted = sortBy (compare `on` snd) modTimes
--     return $ (fst . head) sorted

-- -- expects absolute paths
-- getModTimes    :: [FilePath] -> IO [(FilePath, UTCTime)]
-- getModTimes xs = do
--     modTimes <- foldM (\ts f -> do; modTime <- getModificationTime f; return (ts ++ [modTime])) [] xs
--     return $ zip xs modTimes

-- fixDBusEnvironment :: IO ()
-- fixDBusEnvironment = do
--     maybeVar <- lookup "DBUS_SYSTEM_BUS_ADDRESS" <$> getEnvironment
--     case maybeVar of
--         Just _ -> return ()
--         Nothing -> do
--             -- TODO: error handling!  no pid returned by pgrep!
--             pid <- init <$> readProcess "pgrep" [SremConfig.alwaysRunningInX11Process] ""
--             findDBusEnvironment <$> readFile ("/proc/" ++ pid ++ "/environ")
--                 >>= putStrLn
--                 -- >>= setEnv "DBUS_SYSTEM_BUS_ADDRESS"

-- findDBusEnvironment :: String -> String
-- findDBusEnvironment = maybe "" id
--                       . lookup "DBUS_SYSTEM_BUS_ADDRESS"
--                       . parseEnviron

-- -- | Parse @/proc/n/environ@ where @n@ is a PID.
-- parseEnviron :: String -> [(String, String)]
-- parseEnviron = foldr step [] . splitOn "\NUL"
--   where
--     step env envs =
--         case splitOn "=" env of
--             [key, value] -> (key, value) : envs
--             _            -> envs
