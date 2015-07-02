{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

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

module Utility.CmdLineRem (cmdLineReminder) where

import           Control.Applicative    ((<$>))
import           Data.List              (intercalate)

import           Data.List.Split        (oneOf, split)
import           Data.Modular
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Tuple.Sequence    (sequenceT)
-- import           System.Locale          (defaultTimeLocale)

import           Data.Maybe.Read
import           Types.Reminder
import           Types.Reminder.CmdLine

cmdLineReminder     :: [String] -> IO (Maybe Reminder)
cmdLineReminder []  = return Nothing
cmdLineReminder [_] = return Nothing
cmdLineReminder input@(exp:_) =
    case cmdLineRemType exp of
        Just AbsoluteCLRem ->
            return $ parseAbsoluteRem input
        Just RelativeCLRem -> do
            tod <- localTimeOfDay <$> zonedTimeToLocalTime <$> getZonedTime
            return $ parseRelativeRem tod input
        Nothing            ->
            return Nothing

parseRelativeRem                     :: TimeOfDay -> [String] -> Maybe Reminder
parseRelativeRem tod (exp:textParts) = do
    (h, m) <- parseRelativeTime exp
    let nowH = todHour tod
        nowM = todMin tod
        h'   = nowH + h + ((nowM + m) `div` 60)
        m'   = unMod $ (toMod nowM :: Int/60) + (toMod m :: Int/60)
    makeReminder h' m' text
  where
        text                         = intercalate " " textParts

parseAbsoluteRem                 :: [String] -> Maybe Reminder
parseAbsoluteRem (exp:textParts) = do
    (h, m) <- parseAbsoluteTime exp
    makeReminder h m text
  where
        text                     = intercalate " " textParts

parseRelativeTime :: String -> Maybe (Hour, Minute)
parseRelativeTime exp =
    case (split . oneOf) "mh" exp of
        [h, "h", m, "m", ""] -> sequenceT (readMaybe h, readMaybe m)
        [h, "h", ""]         -> sequenceT (readMaybe h, Just 0)
        [m, "m", ""]         -> sequenceT (Just 0,      readMaybe m)
        [m]                  -> sequenceT (Just 0,      readMaybe m)
        _                    -> Nothing

parseAbsoluteTime     :: String -> Maybe (Hour, Minute)
parseAbsoluteTime exp = let formatString =
                                if last exp == 'm'
                                then "%k:%M%P"
                                else "%k:%M"
                        in parseTime defaultTimeLocale formatString exp
                           >>= \tod -> Just (todHour tod, todMin tod)
