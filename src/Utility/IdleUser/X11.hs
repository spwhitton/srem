{-# LANGUAGE ForeignFunctionInterface #-}

module Utility.IdleUser.X11 (userIdleTime) where

import Foreign
import Foreign.C.Types

foreign import ccall unsafe "GetXIdleTime"
    getXIdleTime :: CUInt

userIdleTime :: Int
userIdleTime = undefined
