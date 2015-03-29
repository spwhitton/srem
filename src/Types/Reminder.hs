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
