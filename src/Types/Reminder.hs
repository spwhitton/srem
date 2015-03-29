module Types.Reminder ( Reminder
                      , makeReminder
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
        (show . to12Hour) h
        ++ ":" ++ (zeroPadTime . show) m
        ++ amPm h ++ " " ++ s

makeReminder :: Hour -> Minute -> String -> Maybe Reminder
makeReminder h m s = if   h `elem` [0..23] && m `elem` [0..59]
                     then return $ Reminder h m s
                     else fail "invalid reminder"

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
