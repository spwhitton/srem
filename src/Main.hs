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

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (mapM_)
import           Data.List           (intercalate)
import           System.Environment  (getArgs)

import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Time.LocalTime
import           System.Locale       (defaultTimeLocale)
import           Text.Regex.Posix    ((=~))

import           Types.Reminder
import           Utility.EventCache
import           Utility.Notify

doCron                  :: IO ()
doCron                  = do
    rems      <- (++) <$> readEmacsEventCache <*> readManualEventCache
    (h, m, _) <- localHMD
    let nowRemsFilter r = getReminderHour r == h
                          && getReminderMinute r == m
        nowRems         = filter nowRemsFilter rems
    mapM_ sendNotification nowRems

cmdLineReminder             :: [String] -> Maybe Reminder
cmdLineReminder []          = Nothing
cmdLineReminder [_]         = Nothing
cmdLineReminder (exp:textParts)
    | exp =~ relativeRegExp :: Bool = do
                                          (h, m) <- parseRelativeTime exp -- TODO: add to current time
                                          makeReminder h m text
    | exp =~ absoluteRegExp :: Bool = do
                                          (h, m) <- parseAbsoluteTime exp
                                          makeReminder h m text
    | otherwise             = Nothing
  where
    text                    = intercalate " " textParts
    relativeRegExp          = "[0-9]+[mh]"
    absoluteRegExp          = "[0-9]{1,2}(:[0-9][0-9])?(am|pm)?"

parseRelativeTime     :: String -> Maybe (Hour, Minute)
parseRelativeTime exp = undefined

parseAbsoluteTime     :: String -> Maybe (Hour, Minute)
parseAbsoluteTime exp = let formatString = if last exp == 'm'
                                           then "%k:%M%P"
                                           else "%k:%M"
                        in parseTime defaultTimeLocale formatString exp
                           >>= \tod -> Just (todHour tod, todMin tod)

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
                else maybe
                     (error "srem: invalid input")
                     appendUserReminder $ cmdLineReminder args

localHMD :: IO (Hour, Minute, Day)
localHMD = do
    localTime <- zonedTimeToLocalTime <$> getZonedTime
    let time = localTimeOfDay localTime
        d    = localDay localTime
        h    = todHour time
        m    = todMin  time
    return (h, m, d)
