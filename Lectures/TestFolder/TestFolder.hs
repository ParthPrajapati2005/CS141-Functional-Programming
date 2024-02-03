module TestFolder where
yeet :: [(Int -> Int)] -> [Int]
yeet [] = []
yeet (f:fs) = f 100 : yeet fs

functionsArr :: [(Int -> Int)]
functionsArr = [(\x -> x + 1), ((\x -> x + 3)), ((\x -> x + 5))]

addPair :: [(Int, Int)] -> [Int]
addPair arr = map (uncurry (+)) arr

foldr' :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)
