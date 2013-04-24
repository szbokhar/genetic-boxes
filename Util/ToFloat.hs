{-# LANGUAGE FlexibleInstances #-}

module Util.ToFloat where

import Data.Maybe   ( fromJust )

class ToFloat a where
    -- |Converts a type to an Int.
    --  Throws an error if it is not possible.
    float  :: a -> Float
    float = fromJust . float'

    -- |Converts a type to a potential int.
    --  Nothing if it is not possible.
    --  Safet version of int
    float' :: a -> Maybe Float

instance ToFloat Int where
    float' = Just . fromIntegral

instance ToFloat Integer where
    float' = Just . fromIntegral

instance ToFloat Double where
    float' = Just . fromRational . toRational

instance ToFloat Float  where
    float' = Just . id

instance ToFloat [Char]  where
    float' str
        | null attempt      = Nothing
        | otherwise         = Just $ (fst . head) attempt
      where attempt = (reads :: ReadS Float) str
