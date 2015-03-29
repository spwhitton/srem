{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Utility.Emacs ( getEmacsOutput
                     , parseEmacsOutput) where

import           Control.Applicative ((<$>))
import           Control.Monad       (foldM, when)
import qualified Control.SremConfig  as SremConfig
import           Data.Maybe.Read
import           Data.Modular
import           System.Directory    (getHomeDirectory)
import           System.FilePath     ((</>))
import           System.Process      (readProcess)
import           Text.Regex.Posix    ((=~))
import           Types.Reminder

parseEmacsOutput :: String -> [Reminder]
parseEmacsOutput = foldr step [] . drop 2 . lines
  where
    step line rems = maybe rems (++ rems) $ parseLine line

parseLine              :: String -> Maybe [Reminder]
parseLine line         = do
    when (not lineMatch) $ fail ""
    let hStr:mStr:s:[] = drop 1 . concat $ lineMatchStrings
    h <- readMaybe hStr
    m <- readMaybe mStr
    staggeredReminders =<< makeReminder h m s
  where
    apptRegexp         = " ([0-9]{1,2}):([0-9][0-9])[-]{0,1}[0-9:]{0,5}[.]* (.*)$"
    snipRegexp         = "[ ]{2,}:.*:*$"

    lineMatch        = line =~ apptRegexp :: Bool
    lineMatchStrings = line =~ apptRegexp :: [[String]]

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
    readProcess "emacs" args ""

makeEmacsArgs      :: String -> [String]
makeEmacsArgs home = [ "-batch"
                     , "-l", home </> ".emacs.d" </> "init.el"
                     , "-eval", "(setq org-agenda-sticky nil)"
                     , "-eval", "(org-batch-agenda \"D\")" ]
