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

import           Control.Applicative    ((<$>), (<*>))
import           Control.Monad          (mapM_)
import           System.Environment     (getArgs)

import           Data.Time.Calendar
import           Data.Time.LocalTime

import           Types.Reminder
import           Types.Reminder.CmdLine
import           Utility.CmdLineRem     (cmdLineReminder)
import           Utility.EventCache
import           Utility.Notify

doCron                  :: IO ()
doCron                  = do
    purgeOldEventCaches
    rems      <- (++) <$> readEmacsEventCache <*> ((++) <$> readManualEventCache <*> getTimetableReminders)
    (h, m, _) <- localHMD
    let nowRemsFilter r = getReminderHour r == h
                          && getReminderMinute r == m
        nowRems         = filter nowRemsFilter rems
    sendNotifications nowRems

appendUserReminder   :: Reminder -> IO ()
appendUserReminder r = do
    (h, m, d) <- localHMD
    -- A reminder doesn't know which day the event is on.  So we make
    -- a dummy reminder for right now, and compare to the user's
    -- input.  If their reminder is for a time that's already passed
    -- today, we save the reminder into tomorrow's cache.

    -- This makes it impossible to set a reminder for more than one
    -- day into the future ('cmdLineReminder' is a pure function and
    -- doesn't know what day it is now).  But the only way to express
    -- a reminder for the day after tomorrow in srem's deliberately
    -- simple command line input would be to write something like
    -- @srem 1500m Go out to meet Tina@ which no user would want to
    -- do.

    -- The day after tomorrow is far enough in the future that an
    -- appointment in Emacs Org-mode should be used instead of srem's
    -- manual reminder input.
    let dummyRem = makeReminder' h m (getReminderText r)
    if maybe False (r <=) dummyRem
        then appendManualEventCache r (addDays 1 d)
        else appendManualEventCache r d

main = do
    args <- getArgs
    if length args == 1 && head args == "--cron"
        then doCron
        else if length args == 1 && head args == "--refresh-emacs"
                then refreshEmacsEventCache
             else cmdLineReminder args
                  >>= maybe (error "invalid input") appendUserReminder

localHMD :: IO (Hour, Minute, Day)
localHMD = do
    localTime <- zonedTimeToLocalTime <$> getZonedTime
    let time = localTimeOfDay localTime
        d    = localDay localTime
        h    = todHour time
        m    = todMin  time
    return (h, m, d)
