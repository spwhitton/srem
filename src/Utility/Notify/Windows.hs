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

module Utility.Notify.Windows (sendNotifications) where

import            System.Environment (getEnv)
import            System.Process     (ProcessHandle, spawnCommand)
import            Control.Monad      (mapM_)

import Types.Reminder

sendNotifications :: [Reminder] -> IO ()
sendNotifications = mapM_ sendNotification

sendNotification   :: Reminder -> IO ProcessHandle
sendNotification r = getEnv "USERNAME" >>= \u ->
    spawnCommand . unwords $
    [ "msg"
    , "/time:5"
    , u
    , getReminderText r
    ]
