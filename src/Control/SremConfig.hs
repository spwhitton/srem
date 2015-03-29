module Control.SremConfig ( getCacheDirectory
                          , intervals
                          ) where

import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

getCacheDirectory :: IO FilePath
getCacheDirectory = getHomeDirectory >>= \h ->
    return $ h </> ".cache" </> "srem"

intervals :: [Int]
intervals = [60, 15, 0]
