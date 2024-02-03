module Jamie where

class Jamiethezestyboi a where
    doublePreet :: a -> a -> Bool
    triplePreet :: a -> a -> Integer

instance Jamiethezestyboi Bool where
    doublePreet True True = True
    doublePreet _ _ = False
    triplePreet True True = 69
    triplePreet _ _ = 0

instance Jamiethezestyboi Integer where
    doublePreet a b = a > b
    triplePreet a b = a + b

five :: (Jamiethezestyboi a) => a -> a -> Bool
five a b = doublePreet a b

six :: (Jamiethezestyboi a) => a -> a -> Bool
six x y = doublePreet x y
