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

module Control.SremConfig ( getCacheDirectory
                          , intervals
                          , notificationSound
                          -- , alwaysRunningInX11Process
                          ) where

import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

-- TODO: check $XDG_CACHE or whatever environment variable before defaulting to ~/.cache

getCacheDirectory :: IO FilePath
getCacheDirectory = getHomeDirectory >>= \h ->
    return $ h </> ".cache" </> "srem"

notificationSound :: IO FilePath
notificationSound = getHomeDirectory >>= \h ->
    return $ h </> "lib" </> "annex" </> "doc" </> "sounds" </> "beep.wav"

intervals :: [Int]
intervals = [60, 15, 0]

-- alwaysRunningInX11Process :: String
-- alwaysRunningInX11Process = "xbindkeys"
