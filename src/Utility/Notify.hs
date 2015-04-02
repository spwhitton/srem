module Utility.Notify (sendNotification) where

import Types.Reminder

sendNotification   :: Reminder -> IO ()
sendNotification r = putStrLn $ show r
