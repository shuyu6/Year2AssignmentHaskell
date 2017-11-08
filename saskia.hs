import Test.QuickCheck
-- Q1a --------------------------------------------------------------------------------

max1 :: Int -> Int -> Int -> Int
max1 x y z | (x >= y), (x >= z) = x
		   | (y >= z) = y
		   | otherwise = z

-- max1 915681 967439 920440 = 967439
maxoftwo :: Int -> Int -> Int
maxoftwo x y = if (x >= y) then x else y

max2 :: Int -> Int -> Int -> Int
max2 x y z = maxoftwo x (maxoftwo y z)

-- max2 915681 967439 920440 = 967439

-- Q1b --------------------------------------------------------------------------------

checkcorrectness x y z = max1 x y z == max2 x y z

{-
+++ OK, passed 100 tests.
 -}
 
-- Q2 ---------------------------------------------------------------------------------

testifsorted :: Ord a => [a] -> Bool
testifsorted [] = True
testifsorted [x] = True
testifsorted (x:y:xs) = (x <= y) && testifsorted (y:xs)

insertelement :: Ord a => a -> [a] -> [a]
insertelement x [] = [x]
insertelememt x (y:ys) = if (x < y) then x:y:ys else y:insertelememt x ys

splitlist :: [a] -> ([a], [a])
splitlist xs = (take ((length xs) `div` 2)  xs, drop ((length xs) `div` 2) xs)

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge (x:xs) (y:ys) | (x < y) = x:merge xs (y:ys)
					| otherwise = y:merge (x:xs) ys

insertionsort :: Ord a => [a] -> [a]
insertionsort [] = []
insertionsort [x] = [x]
insertionsort (x:xs) = insertelememt x (insertionsort xs)

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs | (length xs) > 1 = merge (mergesort ls) (mergesort rs)
			 | otherwise = xs
			 where (ls, rs) = splitlist xs

sortcheck :: [Int] -> Bool
sortcheck xs = if ((insertionsort xs) == (quicksort xs)) && ((quicksort xs) == (mergesort xs)) then True else False

{-
sortcheck [3,67,43,3,8]
-}

-- Q3a --------------------------------------------------------------------------------

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x ==0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primesbelow :: Int -> [Int]
primesbelow n = [x | x <- [2..n], prime x]

-- Q3b --------------------------------------------------------------------------------

allprimes :: [Int]
allprimes = [x | x <- [2..], prime x]

--primetest :: [Bool]