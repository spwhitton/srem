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

module Types.Reminder ( Reminder
                      , makeReminder
                      , makeReminder'
                      , getReminderHour
                      , getReminderMinute
                      , getReminderText
                      ) where

type Hour   = Int
type Minute = Int

data Reminder = Reminder { getReminderHour :: Hour
                         , getReminderMinute :: Minute
                         , getReminderText :: String}
              deriving (Eq, Ord)

instance Show Reminder where
    show (Reminder h m s) =
        "reminder at " ++ niceTime h m ++ ": " ++ s

-- | Maybe makes a reminder for an event, prepending a human-readable
--   time to the event description.
makeReminder       :: Hour           -- ^ Clock hour-hand time of event
                   -> Minute         -- ^ Clock minute-hand time of the event
                   -> String         -- ^ Event description
                   -> Maybe Reminder -- ^ Maybe a reminder
makeReminder h m s = makeReminder' h m (niceTime h m ++ " " ++ s)

-- | Maybe makes a reminder for an event.  Unlike 'makeReminder',
--   doesn't prepend anything to the event description.
makeReminder'       :: Hour           -- ^ Clock hour-hand time of event
                    -> Minute         -- ^ Clock minute-hand time of the event
                    -> String         -- ^ Event description
                    -> Maybe Reminder -- ^ Maybe a reminder
makeReminder' h m s = if   h `elem` [0..23] && m `elem` [0..59]
                      then return $ Reminder h m s
                      else fail "invalid reminder"

niceTime :: Hour -> Minute -> String
niceTime h m = (show . to12Hour) h
               ++ ":" ++ (zeroPadTime . show) m
               ++ amPm h

zeroPadTime   :: String -> String
zeroPadTime t = if   length t < 2
                then '0' : t
                else t

to12Hour :: Int -> Int
to12Hour h = if   h > 12
             then h - 12
             else h

amPm :: Int -> String
amPm h = if   h > 12
         then "pm"
         else "am"
