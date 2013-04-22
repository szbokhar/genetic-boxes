{-# LANGUAGE FlexibleInstances #-}

module Util.ToInt where

import Data.Maybe   ( fromJust )

class ToInt a where
    -- |Converts a type to an Int.
    --  Throws an error if it is not possible.
    int  :: a -> Int
    int = fromJust . int'

    -- |Converts a type to a potential int.
    --  Nothing if it is not possible.
    --  Safet version of int
    int' :: a -> Maybe Int

instance ToInt [Char]  where
    int' str
        | null attempt      = Nothing
        | otherwise         = Just $ (fst . head) attempt
      where attempt = (reads :: ReadS Int) str
