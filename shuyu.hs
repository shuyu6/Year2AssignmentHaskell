--Group members 
--Student no. : 967439	Name : Goh Shu Yu
--920440
--915681

import Test.QuickCheck

--Question 1
max1 :: Int -> Int -> Int -> Int
max1 x y z 
	| x >= y && x >=z = x
	| y >=z = y
	|otherwise 		  = z


max2 :: Int -> Int -> Int -> Int
max2 x y z = if x>=y && x>=z then x
			else if y>=z then y
			else z

-- Question 1 b
checkcorrectness x y z = max1 x y z == max2 x y z

-- Question 2
checkSorted :: Ord a => [a] -> Bool
checkSorted [] = True
checkSorted [x] = True
checkSorted (x:xs) = checkElement x xs


checkElement :: Ord a => a -> [a] -> Bool
checkElement x [] = True
checkElement x (y:ys) = if x < y then checkElement y ys
			else False


insertElement :: Ord a => a -> [a] -> [a]
insertElement x [] = [x]
insertElement x (y:ys) = if x < y then x:y:ys
			else y:insertElement x ys



insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x:xs) = insertElement x (insertionSort xs)

--Question 3
--a
factors :: Int -> [Int]
factors n = [ x | x <- [1..n] , n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primesbelow :: Int -> [Int]
primesbelow n = [x | x <- [2..n], prime x]

--b
allprimes :: [Int]
allprimes = [x | x<- [2..], prime x]

primeTest :: [Bool]
primeTest = [ prime x| x <- [1..]]

--c /  d
isPrimeTwins :: (Int,Int) -> Bool
isPrimeTwins (x,y) = y-x == 2 

primeTwinsListBelow :: Int -> [(Int,Int)]
primeTwinsListBelow n = [ (allprimes!!y,allprimes!!(y+1))| y<-[1..n-1]]

-- primeTwinsTestIs :: [(Int,Int)]->Int
-- primeTwinsTestIs n = [x | ]

countPrimeTwins :: Int -> Int
countPrimeTwins n = length (filter isPrimeTwins (primeTwinsListBelow n))

-- question 4
visitedCities :: Int -> [Int]
visitedCities n = if n == 1 then [n]
			else if even n then [n]++visitedCities(n `div` 2)
			else [n]++visitedCities (n*3+1)

numCitiesVisited = \n-> length (visitedCities n)


findLargestCities :: [Int] -> Int
findLargestCities [] = 0
findLargestCities [x] = x
findLargestCities (x:xs) = max x (findLargestCities xs)

myLargestCities = \x -> findLargestCities (visitedCities x)                               