{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}

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

module Utility.Emacs ( getEmacsOutput
                     , parseEmacsOutput) where

import           Control.Applicative ((<$>))
import           Control.Monad       (foldM, when)
import qualified Control.SremConfig  as SremConfig
import           Data.Maybe.Read
import           Data.Modular
import           System.Directory    (getHomeDirectory)
import           System.FilePath     ((</>))
import           System.Process      (readProcessWithExitCode)
import           Text.Regex.Posix    ((=~))
import           Types.Reminder

parseEmacsOutput :: String -> [Reminder]
parseEmacsOutput = foldr step [] . drop 2 . lines
  where
    step line rems = maybe rems (++ rems) $ parseLine line

parseLine              :: String -> Maybe [Reminder]
parseLine line         = do
    when (not lineMatch) $ fail ""
    let hStr:mStr:sStr:[] = drop 1 . concat $ lineMatchStrings
    h <- readMaybe hStr
    m <- readMaybe mStr
    let s = snip sStr
    staggeredReminders =<< makeReminder h m s
  where
    apptRegexp         = " ([0-9]{1,2}):([0-9][0-9])[-]{0,1}[0-9:]{0,5}[.]* (.*)$"
    snipRegexp         = "[ ]{1,}:.*:*$"

    lineMatch        = line =~ apptRegexp :: Bool
    lineMatchStrings = line =~ apptRegexp :: [[String]]

    fst3 (x, _, _) = x
    snip s = fst3 (s =~ snipRegexp :: (String, String, String))

staggeredReminders   :: Reminder -> Maybe [Reminder]
staggeredReminders r = sequence $ foldr step [] SremConfig.intervals
  where
    step minsBefore rems =
        makeReminder' (h minsBefore) (m minsBefore) (getReminderText r) : rems
    h m = if   m > getReminderMinute r
          then getReminderHour r - 1
          else getReminderHour r
    m m = unMod $ (toMod (getReminderMinute r) :: Int/60) - (toMod m :: Int/60)

getEmacsOutput :: IO String
getEmacsOutput = do
    args <- makeEmacsArgs <$> getHomeDirectory

    -- use 'readProcessWithExitCode' over 'readProcess' to avoid
    -- printing Emacs startup verbose stderr to the user
    (_, output, _) <- readProcessWithExitCode "emacs" args ""
    return output

makeEmacsArgs      :: String -> [String]
makeEmacsArgs home = [ "-batch"
                     , "-l", home </> ".emacs.d" </> "init.el"
                     , "-eval", "(setq org-agenda-sticky nil)"
                     , "-eval", "(org-batch-agenda \"D\")" ]
