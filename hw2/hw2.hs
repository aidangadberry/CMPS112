-- Assignment 2 - 1/24/16 - Aidan Gadberry agadberr

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl op acc [] = acc
myFoldl op acc (x:xs) = myFoldl op (acc `op` x) xs

myReverse :: [a] -> [a]  
myReverse = foldl (\x y -> y : x) []

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr op acc xs = foldl (\x y -> y `op` x) acc (reverse xs)

myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 op acc xs = foldr (\x y -> y `op` x) acc (reverse xs)

isUpper :: Char -> Bool
isUpper x = elem x ['A'..'Z']

onlyCapitals1 :: String -> String
onlyCapitals1 xs = filter isUpper xs

onlyCapitals2 :: String -> String
onlyCapitals2 xs = [x | x <- xs, isUpper x] 

onlyCapitals3 :: String -> String
onlyCapitals3 [] = []
onlyCapitals3 (x:xs)
    | isUpper x == True = x : (onlyCapitals3 xs)
    | isUpper x == False = onlyCapitals3 xs

divRemainder :: Int -> Int -> (Int, Int)
divRemainder x y = (x `div` y, x `mod` y)

digitSum :: Int -> Int
digitSum 0 = 0
digitSum x = x `mod` 10 + digitSum (x `div` 10)

sayPrefix :: Integer -> String
sayPrefix 0 = ""
sayPrefix x
    | x == 1 = "one "
    | x == 2 = "two "
    | x == 3 = "three "
    | x == 4 = "four "
    | x == 5 = "five "
    | x == 6 = "six "
    | x == 7 = "seven "
    | x == 8 = "eight "
    | x == 9 = "nine "
    | x == 10 = "ten "
    | x == 11 = "eleven "
    | x == 12 = "twelve "
    | x == 13 = "thirteen "
    | x == 14 = "fourteen "
    | x == 15 = "fifteen "
    | x == 16 = "sixteen "
    | x == 17 = "seventeen "
    | x == 18 = "eighteen "
    | x == 19 = "nineteen "
    | x `div` 10 == 2 = "twenty " ++ sayPrefix (x `mod` 10)
    | x `div` 10 == 3 = "thirty " ++ sayPrefix (x `mod` 10)
    | x `div` 10 == 4 = "forty " ++ sayPrefix (x `mod` 10)
    | x `div` 10 == 5 = "fifty " ++ sayPrefix (x `mod` 10)
    | x `div` 10 == 6 = "sixty " ++ sayPrefix (x `mod` 10)
    | x `div` 10 == 7 = "seventy " ++ sayPrefix (x `mod` 10)
    | x `div` 10 == 8 = "eighty " ++ sayPrefix (x `mod` 10)
    | x `div` 10 == 9 = "ninety " ++ sayPrefix (x `mod` 10)
    | x `div` 100 > 0 = sayPrefix (x `div` 100) ++ "hundred " ++ sayPrefix (x `mod` 100)
-- used to translate the prefixes (from 1-999) of each 10^3 increment in sayNum

sayNum :: Integer -> String
sayNum 0 = ""
sayNum x
    | x `div` u > 0 = sayPrefix (x `div` u) ++ "vigintillion " ++ sayNum (x `mod` u)
    | x `div` t > 0 = sayPrefix (x `div` t) ++ "novemdecillion " ++ sayNum (x `mod` t)
    | x `div` s > 0 = sayPrefix (x `div` s) ++ "octodecillion " ++ sayNum (x `mod` s)
    | x `div` r > 0 = sayPrefix (x `div` r) ++ "septendecillion " ++ sayNum (x `mod` r)
    | x `div` q > 0 = sayPrefix (x `div` q) ++ "sexdecillion " ++ sayNum (x `mod` q)
    | x `div` p > 0 = sayPrefix (x `div` p) ++ "quindecillion " ++ sayNum (x `mod` p)
    | x `div` o > 0 = sayPrefix (x `div` o) ++ "quattuordecillion " ++ sayNum (x `mod` o)
    | x `div` n > 0 = sayPrefix (x `div` n) ++ "tredecillion " ++ sayNum (x `mod` n)
    | x `div` m > 0 = sayPrefix (x `div` m) ++ "duodecillion " ++ sayNum (x `mod` m)
    | x `div` l > 0 = sayPrefix (x `div` l) ++ "undecillion " ++ sayNum (x `mod` l)
    | x `div` k > 0 = sayPrefix (x `div` k) ++ "decillion " ++ sayNum (x `mod` k)
    | x `div` j > 0 = sayPrefix (x `div` j) ++ "nonillion " ++ sayNum (x `mod` j)
    | x `div` i > 0 = sayPrefix (x `div` i) ++ "octillion " ++ sayNum (x `mod` i)
    | x `div` h > 0 = sayPrefix (x `div` h) ++ "septillion "  ++ sayNum (x `mod` h)
    | x `div` g > 0 = sayPrefix (x `div` g) ++ "sextillion " ++ sayNum (x `mod` g)
    | x `div` f > 0 = sayPrefix (x `div` f) ++ "quintillion " ++ sayNum (x `mod` f)
    | x `div` e > 0 = sayPrefix (x `div` e) ++ "quadrillion " ++ sayNum (x `mod` e)
    | x `div` d > 0 = sayPrefix (x `div` d) ++ "trillion " ++ sayNum (x `mod` d)
    | x `div` c > 0 = sayPrefix (x `div` c) ++ "billion " ++ sayNum (x `mod` c)
    | x `div` b > 0 = sayPrefix (x `div` b) ++ "million " ++ sayNum (x `mod` b)
    | x `div` a > 0 = sayPrefix (x `div` a) ++ "thousand " ++ sayNum (x `mod` a)
    | x `div` a == 0 = sayPrefix x
    where {a = 1000; u = 1000000000000000000000000000000000000000000000000000000000000000;
           b = 1000000; t = 1000000000000000000000000000000000000000000000000000000000000;
           c = 1000000000; s = 1000000000000000000000000000000000000000000000000000000000;
           d = 1000000000000; r = 1000000000000000000000000000000000000000000000000000000;
           e = 1000000000000000; q = 1000000000000000000000000000000000000000000000000000;
           f = 1000000000000000000; p = 1000000000000000000000000000000000000000000000000;
           g = 1000000000000000000000; o = 1000000000000000000000000000000000000000000000; 
           h = 1000000000000000000000000; n = 1000000000000000000000000000000000000000000;
           i = 1000000000000000000000000000; m = 1000000000000000000000000000000000000000;
           j = 1000000000000000000000000000000; l = 1000000000000000000000000000000000000;
           k = 1000000000000000000000000000000000}
-- adds a prefix to each suffix that the given number includes, by recursively going down
-- the list and checking if the given number returns a result greater than zero when 
-- divided by that power of 10
