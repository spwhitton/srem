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

import           Control.Applicative ((<$>), (<*>))
import           System.Environment  (getArgs)
import           Types.Reminder
import           Utility.EventCache

doCron :: IO ()
doCron = undefined

cmdLineReminder      :: [String] -> Maybe Reminder
cmdLineReminder args = undefined

appendUserReminder   :: Reminder -> IO ()
appendUserReminder r = undefined

main = do
    args <- getArgs
    if length args == 1 && head args == "--cron"
        then doCron
        else if length args == 1 && head args == "--emacs"
                then refreshEmacsEventCache
                else maybe
                     (error "srem: invalid input")
                     appendUserReminder $ cmdLineReminder args
