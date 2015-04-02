module Utility.CmdLineRem (cmdLineReminder) where

import Types.Reminder.CmdLine

import           Data.List              (intercalate)
import           Control.Applicative    ((<$>))

import           Data.Tuple.Sequence    (sequenceT)
import           System.Locale          (defaultTimeLocale)
import           Data.List.Split        (oneOf, split)
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Time.LocalTime

import           Types.Reminder
import           Data.Maybe.Read

cmdLineReminder     :: [String] -> IO (Maybe Reminder)
cmdLineReminder []  = return Nothing
cmdLineReminder [_] = return Nothing
cmdLineReminder input@(exp:textParts) =
    case cmdLineRemType exp of
        Just AbsoluteCLRem ->
            return $ parseAbsoluteRem input
        Just RelativeCLRem -> do
            tod <- localTimeOfDay <$> zonedTimeToLocalTime <$> getZonedTime
            return $ parseRelativeRem tod input
        Nothing            ->
            return Nothing
  where
        text        = intercalate " " textParts

parseRelativeRem :: TimeOfDay -> [String] -> Maybe Reminder
parseRelativeRem = undefined

parseAbsoluteRem :: [String] -> Maybe Reminder
parseAbsoluteRem = undefined

parseRelativeTime :: String -> Maybe (Hour, Minute)
parseRelativeTime exp =
    case (split . oneOf) "mh" exp of
        [h, "h", m, "m", ""] -> sequenceT (readMaybe h, readMaybe m)
        [h, "h", ""]         -> sequenceT (readMaybe h, Just 0)
        [m, "m", ""]         -> sequenceT (Just 0,      readMaybe m)
        _                    -> Nothing

parseAbsoluteTime     :: String -> Maybe (Hour, Minute)
parseAbsoluteTime exp = let formatString =
                                if last exp == 'm'
                                then "%k:%M%P"
                                else "%k:%M"
                        in parseTime defaultTimeLocale formatString exp
                           >>= \tod -> Just (todHour tod, todMin tod)
