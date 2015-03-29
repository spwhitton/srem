module Utility.Emacs ( getEmacsOutput
                     , parseEmacsOutput) where

import           Control.Applicative ((<$>))
import           Control.Monad       (when, foldM)
import           Data.Maybe.Read
import           System.Directory    (getHomeDirectory)
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
staggeredReminders r = undefined

-- staggeredReminders :: Int -> Int -> String -> [Reminder]
-- staggeredReminders hour mins text = foldl' step [] [60, 15, 0]
--   where step rems diff = let hour'
--                                | diff > mins = hour - 1
--                                | otherwise = hour
--                              -- could do with addition mod 60
--                              mins'
--                                | mins >= diff = mins - diff
--                                | otherwise = 60 + (mins - diff)
--                          in rems ++ [Reminder hour' mins' text]

getEmacsOutput :: IO String
getEmacsOutput = do
    args <- makeEmacsArgs <$> getHomeDirectory
    readProcess "emacs" args ""

makeEmacsArgs      :: String -> [String]
makeEmacsArgs home = [ "-batch"
                     , "-l", home ++ "/.emacs.d/init.el"
                     , "-eval", "(setq org-agenda-sticky nil)"
                     , "-eval", "(org-batch-agenda \"D\")" ]
