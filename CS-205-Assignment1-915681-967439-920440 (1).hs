import Test.QuickCheck
import Data.Char
import System.Directory
import Data.List.Split
-- Question 1 a ---------------------------------------------------------------------------------

max1 :: Int -> Int -> Int -> Int
max1 a b c = if a >= b && a >= c then a
             else if b >= c then b
             else c

max2 :: Int -> Int -> Int -> Int
max2 a b c | a >= b && a >= c = a
           | b >= c = b
           | otherwise = c

-- *Main> max1 915681 967439 920440
-- 967439
-- *Main> max2 915681 967439 920440
-- 967439

-- Question 1 b
checkcorrectness :: Int -> Int -> Int -> Bool
checkcorrectness x y z = max1 x y z == max2 x y z
-- response: +++ OK, passed 100 tests
-- A drawback of this test is that it only tests for 100 values and assumes correct beyond this.
-- There may be invalid integer combinations beyond this (and below it, in the case of negative numbers) that are not accounted for.

-- Question 2   ---------------------------------------------------------------------------------

testifsorted :: Ord a => [a] -> Bool
testifsorted [] = True
testifsorted [x] = True
testifsorted (x:y:ys) = (x <= y) && testifsorted (y:ys)
-- *Main> testifsorted [321,54,654,21,46]
-- False
-- *Main> testifsorted [123,234,564,687]
-- True

insertelememt :: Ord a => a -> [a] -> [a]
insertelememt x [] = [x]
insertelememt x (y:ys) = if x < y then x:y:ys
            else y:insertelememt x ys

splitlist :: [a] -> ([a], [a])
splitlist xs = (take ((length xs) `div` 2)  xs, drop ((length xs) `div` 2) xs)

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys) | (x < y) = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

insertionsort :: Ord a => [a] -> [a]
insertionsort [] = []
insertionsort [x] = [x]
insertionsort (x:xs) = insertelememt x (insertionsort xs)
--this is insertionsort here is the example result of insertion sort 
-- *Main> insertionsort [321,54,3215,325,21,2]
-- [2,21,54,321,325,3215]

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]
-- this is a quicksort and here is the example result of the sorting algorithm 
-- *Main> quicksort [32,54,321,84,32,4132]
-- [32,32,54,84,321,4132]

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs | (length xs) > 1 = merge (mergesort ls) (mergesort rs)
             | otherwise = xs
             where (ls, rs) = splitlist xs
-- this is a mergesort and below shown the sample input and output 
-- *Main> mergesort [12,35,21,61,32,84,31,5132,54]
-- [12,21,31,32,35,54,61,84,5132]

sortcheck :: [Int] -> Bool
sortcheck xs = if ((insertionsort xs) == (quicksort xs)) && ((quicksort xs) == (mergesort xs)) then testifsorted(mergesort xs) else False
-- this function is used to check above 3 algorithms whether have the same output which is sorted with same input int array
-- *Main> sortcheck [321,54,321,5,25]
-- True

-- Question 3   ---------------------------------------------------------------------------------
--a
factors :: Integer -> [Integer]
factors n = [ x | x <- [1..n] , n `mod` x == 0]

prime :: Integer -> Bool
prime n = factors n == [1,n]

primesbelow :: Integer -> [Integer]
primesbelow n = [x | x <- [2..n], prime x]
-- primesbelow will show all prime numbers which are below 5
-- *Main> primesbelow 5
-- [2,3,5]

--b
allprimes :: [Integer]
allprimes = [x | x<- [2..], prime x]

primeTest :: [Bool]
primeTest = [ prime x| x <- [1..]]

-- this produce an infinite list such that the nth position is the pair (n,b) where the value b is True if n is prime and False otherwise.
-- exm = [(1,False),(2,True),(3,True),(4,False),(5,True),(6,False),(7,True),(8,False),(9,False)
-- the list is start from integer 1
primeList = zip [1..] primeTest

--c 
--this use to validate whether the list is a primeTwins 
isPrimeTwins :: (Integer,Integer) -> Bool
isPrimeTwins (x,y) = y-x == 2 

--this is used to list out nth prime list 
primeListBelow :: Int -> [(Integer,Integer)]
primeListBelow n = [(allprimes!!y,allprimes!!(y+1))| y<-[1..n-1]]

-- calculate the number of prime twins amoungst the first n prime numbers
countPrimeTwins :: Int -> Int
countPrimeTwins n = length (filter isPrimeTwins (primeListBelow n))

-- ans d) prim twins are there amongst the first 2000 prime numbers
-- *Main> countPrimeTwins 2000
-- 302 

-- Question 4   ---------------------------------------------------------------------------------
-- this is the function to list out all cities has been visited on the journey.
visitedCities :: Int -> [Int]
visitedCities n = if n == 1 then [n]
            else if even n then [n]++visitedCities(n `div` 2)
            else [n]++visitedCities (n*3+1)

-- *Main> visitedCities 967439
-- [967439,2902318,1451159,4353478,2176739,6530218,3265109,9795328,4897664,2448832,
-- 1224416,612208,306104,153052,76526,38263,114790,57395,172186,86093,258280,129140,
-- 64570,32285,96856,48428,24214,12107,36322,18161,54484,27242,13621,40864,20432,10216,
-- 5108,2554,1277,3832,1916,958,479,1438,719,2158,1079,3238,1619,4858,2429,7288,3644,1822,
-- 911,2734,1367,4102,2051,6154,3077,9232,4616,2308,1154,577,1732,866,433,1300,650,325,976,
-- 488,244,122,61,184,92,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]

-- *Main> visitedCities 915681
-- [915681,2747044,1373522,686761,2060284,1030142,515071,1545214,772607,2317822,1158911,3476734,
-- 1738367,5215102,2607551,7822654,3911327,11733982,5866991,17600974,8800487,26401462,13200731,
-- 39602194,19801097,59403292,29701646,14850823,44552470,22276235,66828706,33414353,100243060,
-- 50121530,25060765,75182296,37591148,18795574,9397787,28193362,14096681,42290044,21145022,
-- 10572511,31717534,15858767,47576302,23788151,71364454,35682227,107046682,53523341,160570024,
-- 80285012,40142506,20071253,60213760,30106880,15053440,7526720,3763360,1881680,940840,470420,
-- 235210,117605,352816,176408,88204,44102,22051,66154,33077,99232,49616,24808,12404,6202,3101,
-- 9304,4652,2326,1163,3490,1745,5236,2618,1309,3928,1964,982,491,1474,737,2212,1106,553,1660,830,
-- 415,1246,623,1870,935,2806,1403,4210,2105,6316,3158,1579,4738,2369,7108,3554,1777,5332,2666,1333,
-- 4000,2000,1000,500,250,125,376,188,94,47,142,71,214,107,322,161,484,242,121,364,182,91,274,137,412,
-- 206,103,310,155,466,233,700,350,175,526,263,790,395,1186,593,1780,890,445,1336,668,334,167,502,251,
-- 754,377,1132,566,283,850,425,1276,638,319,958,479,1438,719,2158,1079,3238,1619,4858,2429,7288,3644,
-- 1822,911,2734,1367,4102,2051,6154,3077,9232,4616,2308,1154,577,1732,866,433,1300,650,325,976,488,244,
-- 122,61,184,92,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]

-- *Main> visitedCities 920440
-- [920440,460220,230110,115055,345166,172583,517750,258875,776626,388313,1164940,582470,291235,873706,436853,
-- 1310560,655280,327640,163820,81910,40955,122866,61433,184300,92150,46075,138226,69113,207340,103670,51835,
-- 155506,77753,233260,116630,58315,174946,87473,262420,131210,65605,196816,98408,49204,24602,12301,36904,18452,
-- 9226,4613,13840,6920,3460,1730,865,2596,1298,649,1948,974,487,1462,731,2194,1097,3292,1646,823,2470,1235,3706,
-- 1853,5560,2780,1390,695,2086,1043,3130,1565,4696,2348,1174,587,1762,881,2644,1322,661,1984,992,496,248,124,62,
-- 31,94,47,142,71,214,107,322,161,484,242,121,364,182,91,274,137,412,206,103,310,155,466,233,700,350,175,526,263,
-- 790,395,1186,593,1780,890,445,1336,668,334,167,502,251,754,377,1132,566,283,850,425,1276,638,319,958,479,1438,
-- 719,2158,1079,3238,1619,4858,2429,7288,3644,1822,911,2734,1367,4102,2051,6154,3077,9232,4616,2308,1154,577,1732,
-- 866,433,1300,650,325,976,488,244,122,61,184,92,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]

-- this is the function to calculate the total cities have been visited throughout the journey
totalCitiesVisited = \n-> length (visitedCities n)

-- *Main> totalCitiesVisited 967439
-- 96
-- *Main> totalCitiesVisited 915681
-- 233
-- *Main> totalCitiesVisited 920440
-- 202

-- this function is used to find the largest city's number according to a list 
findLargestCities :: [Int] -> Int
findLargestCities [] = 0
findLargestCities [x] = x
findLargestCities (x:xs) = max x (findLargestCities xs)

-- this function is used to show the largest city's number according to the given number 
theLargestCityVisited = \x -> findLargestCities (visitedCities x)        

-- *Main> theLargestCityVisited 967439
-- 9795328
-- *Main> theLargestCityVisited 915681
-- 160570024
-- *Main> theLargestCityVisited 920440
-- 1310560

-- Question 5   ---------------------------------------------------------------------------------

--Note: The reading from the file did work in concept somewhat, but we couldn't find a solution to obtaining the parent filepath
--to work this, uncomment the code and change the filepath to point to the location of the surnames file

{--
surnameList = do
  fileString <- readFile "surnames.txt"
  let splitted = splitOn "\n" fileString
  
  -- let mapped = map ignoreNonAlphabetic splitted
  -- let finalArray = matchingNameArray splitted
  print splitted
--}

surnames = ["Smith","Smyth","Smythe","Smid","Schmidt","Smithers","Jonas","Johns","Johnson","Macdonald","Nest O'Malett","Ericsson","Erikson","Saunas","Van Damme"]
dischargeLetters = "aeihouwy"
equivalentLetter = [('1',"aeiou"),
                    ('2',"cgjkqsxyz"),
                    ('3',"bfpvw"),
                    ('4',"dt"),
                    ('5',"mn")]

-- [("Smith","254"),("Smyth","254"),("Smythe","254"),("Smythe","254"),("Schmidt","254"),("Smithers","25462"),
-- ("Jonas","252"),("Johns","252"),("Johnson","2525"),("Macdonald","524564"),("Nest O'Malett","5241564"),
-- ("Ericsson","1625"),("Erikson","1625"),("Saunas","252"),("Van Damme","3545")]

--this function is used to remove non alphabetic character 
ignoreNonAlphabetic :: String -> String
ignoreNonAlphabetic xs = [ x | x<-xs, isLetter x]

-- this function used to convert the given letter to it's classed number 
convertChar :: Char -> Char
convertChar c = convertChar' c equivalentLetter
    where
      convertChar' _ []       = '6'
      convertChar' c ((cs,cls):clss)
         | c `notElem` cls = convertChar' c clss
         | otherwise      = cs

-- this function used to discharge letter AEIHOUWY after the first letter 
dischargeLetterFunction :: String -> String
dischargeLetterFunction (x:xs) = x:[r | r <- (ignoreNonAlphabetic xs), r `notElem` dischargeLetters ]


-- this function is used to remove the consecutive character in a string 
filterConsecutive :: String -> String
filterConsecutive [] = []
filterConsecutive [x] = [x]
filterConsecutive (x1:x2:xs)
    | x1==x2 = filterConsecutive (x2:xs)
    | otherwise = x1:filterConsecutive(x2:xs)

-- this function is used to convert the user name to classed string 
changeString :: String -> String
changeString xs = [ convertChar (toLower temp) | temp <-(dischargeLetterFunction xs)]

-- this is used to convert the string array to the classed string 
filterNames :: [String] -> [String]
filterNames xs = [ filterConsecutive (changeString x) | x<-xs ]

-- find matched pattern  
matchingName :: String -> [String]
matchingName xs = matchingName' xs (zip surnames (filterNames surnames))
    where
      matchingName' _ []       = []
      matchingName' xs ((name,pattern):restName)
         | (changeString xs) == pattern = [name] ++ matchingName' xs restName
         | otherwise      = matchingName' xs restName

matchingNameArray :: [String] -> [[String]]
matchingNameArray xs = [ matchingName x | x<-xs]

-- *Main> matchingNameArray ["Jones","Winton"]
-- [["Jonas","Johns","Saunas"],["Van Damme"]]

