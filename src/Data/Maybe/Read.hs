module Data.Maybe.Read (readMaybe) where

-- readMaybe function from "Learn You a Haskell"

readMaybe    :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
    [(x,"")] -> Just x
    _        -> Nothing
