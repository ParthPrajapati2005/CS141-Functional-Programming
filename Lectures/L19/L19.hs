module L19 where

instance Show (a -> b) where
    show :: (a -> b) -> String
    show f = "<function>"