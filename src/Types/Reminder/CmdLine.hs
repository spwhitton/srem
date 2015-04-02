module Types.Reminder.CmdLine where

import           Text.Regex.Posix ((=~))

data CmdLineRemType = RelativeCLRem | AbsoluteCLRem
                    deriving (Eq, Show, Ord, Bounded)

cmdLineRemType              :: String -> Maybe CmdLineRemType
cmdLineRemType exp
    | exp =~ relativeRegExp = Just RelativeCLRem
    | exp =~ absoluteRegExp = Just AbsoluteCLRem
    | otherwise             = Nothing
  where
    relativeRegExp          = "[0-9mh]+[mh]"
    absoluteRegExp          = "[0-9]{1,2}(:[0-9][0-9])?(am|pm)?"
