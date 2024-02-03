module L9 where
import Language.Haskell.TH (safe)

--not  :: Bool -> Bool
--not True = False
--not False = True

data RPSThrow = Rock | Paper | Scissors --deriving Show

instance Show RPSThrow where
    show Rock = "R"
    show Paper = "P"
    show Scissors = "S"

beat :: RPSThrow -> RPSThrow
beat Rock = Paper
beat Paper = Scissors
beat Scissors = Rock

data Date 
    = BC Int Int Int -- :type Date is Int -> Int -> Int ->  Date
    | AD Int Int Int
    deriving Show

firstOfJan:: Int -> Date
firstOfJan y = AD 1 1 y

safeHead :: [a] -> Maybe a
safeHead [] = Nothing        -- Just like True/False are of the Bool data type, Nothing and Just x are both from the Maybe data type.
safeHead (x:_) = Just x

safeDiv :: Integral a => a -> a -> Maybe a
safeDiv x y = if y == 0 then Nothing else Just (x `div` y)