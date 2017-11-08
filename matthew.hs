import Test.QuickCheck

-- 1.)

max1 :: Int -> Int -> Int -> Int
max1 a b c = if a >= b && a >= c then a
             else if b >= c then b
             else c

max2 :: Int -> Int -> Int -> Int
max2 a b c | a >= b && a >= c = a
           | b >= c = b
           | otherwise = c

checkcorrectness :: Int -> Int -> Int -> Bool
checkcorrectness x y z = max1 x y z == max2 x y z

-- response: +++ OK, passed 100 tests
-- A drawback of this test is that it only tests for 100 values and assumes correct beyond this.
-- There may be invalid integer combinations beyond this (and below it, in the case of negative numbers) that are not accounted for.

-- 2.)

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:ys) = if x > y then False else isSorted (y:ys)

insertionsort :: Ord a => [a] -> [a]
insertElement :: Ord a => a -> [a] -> [a]

insertElement x [] = [x]
insertElement x (y:ys) = if (x < y) then [x] ++ y:ys else [y] ++ insertElement x ys
insertionsort [] = []
insertionsort (x:xs) = insertElement x (insertionsort xs)

qsort       :: [Int] -> [Int]
qsort []     = []
qsort (x:xs) =
   qsort smaller ++ [x] ++ qsort larger
   where
      smaller = [a | a <- xs, a <= x]
      larger  = [b | b <- xs, b > x]

-- note: quicksort (qsort) is as copied from the coursework notes powerpoint

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge (x:xs) [] = (x:xs)
merge [] (y:ys) = (y:ys)
merge (x:xs) (y:ys) = merge xs (insertElement x (y:ys))

msort :: [Int] -> [Int]
msort x = | length x <= 1 = x
          | merge(qsort 
