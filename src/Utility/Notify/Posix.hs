module Utility.Notify (sendNotifications) where

import           Control.Applicative ((<$>))
import           Control.Exception   (catch)
import           System.Environment  (getEnvironment, setEnv)
import           System.Process      (readProcess, runCommand)

import           DBus                (Address)
import           DBus.Client         (Client, ClientError, connect)
import           DBus.Notify

import qualified Control.SremConfig  as SremConfig
import           Data.List.Split     (splitOn)
import           Types.Reminder

sendNotifications   :: [Reminder] -> IO ()
sendNotifications rems = do
    client <- getDBusUserAddress >>= connect
              -- `catch` ((\_ -> return ()) :: ClientError -> IO Client) -- TODO
    mapM_ (sendNotification client) rems

sendNotification       :: Client -> Reminder -> IO Notification
sendNotification c rem = do
    let (title:content) = words . getReminderText $ rem
    soundFile <- SremConfig.notificationSound
    runCommand $ "aplay " ++ soundFile
    notify c blankNote { appName = "srem"
                                            , body    = (Just . Text . unwords) content
                                            , summary = title
                                            , expiry  = Milliseconds 10000
                                            , hints   = [ SoundFile soundFile ]}

getDBusUserAddress :: IO Address
getDBusUserAddress = undefined

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
