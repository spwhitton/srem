module Types.Reminder.CmdLine where

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
