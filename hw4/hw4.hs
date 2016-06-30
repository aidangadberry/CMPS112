-- Assignment 4 - 2/28/16 - Aidan Gadberry agadberr

{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances #-}

import System.Random
import Data.Int
import Control.Monad

-- #1
class (Show a) => Gen a where
    gen :: IO a

instance (Show a, Random a) => Gen a where
    gen = randomIO

instance (Gen a, Gen b) => Gen (a, b) where
    gen = do
        n0 <- gen
        n1 <- gen
        return (n0, n1)

instance (Gen a) => Gen [a] where
    gen = do
        n <- randomRIO(0,10)
        replicateM n gen

{--- #2

class Testable a where
    test :: a -> IO Bool

instance Testable Bool where
    test b = return b

instance (Gen a, Testable b) => Testable (a -> b) where
    test t = do
        n0 <- gen
        test (t n0)

-- #3

quickCheck :: (Testable a) => Int -> a -> IO ()
quickCheck n t = do
    check <- test t
    if (n > 0) then
        if (check == True) then
            quickCheck (n-1) t
        else
            putStrLn ("Failing")
    else
        return ()-}

-- #4

class Testable a where
    test :: String -> a -> IO (Bool, String)

instance Testable Bool where
    test str b = return (b, str)

instance (Gen a, Testable b) => Testable (a -> b) where
    test str t = do
        n0 <- gen
        if (str == "") then
            test (show n0) (t n0)
        else
            test (str ++ " " ++ (show n0)) (t n0)

quickCheck :: (Testable a) => Int -> a -> IO ()
quickCheck n t = do
    (check, str) <- test "" t
    if (n > 0) then
        if (check == True) then
            quickCheck (n-1) t
        else
            putStrLn ("Failing inputs = " ++ str)
    else
        return ()

-- #5

-- #6

isort :: [Int8] -> [Int8]
isort [] = []
isort (x:xs) = insert (isort xs)
  where insert [] = [x]
        insert (h:t) | x > h = h:insert t
                     | x <= h = x:h:t
                    -- the x and h needed to be swapped. since x <= h, doing
                    -- h:x:t doesnt make sense. changing it to x:h:t ensures
                    -- the correct ordering

qsort :: [Int8] -> [Int8]
qsort [] = []
qsort (x:xs) = qsort [a | a <- xs, a < x] ++ [x] ++ qsort [a | a <- xs, a >= x]
                    -- the qsort would mess up if a == x, so there needs to 
                    -- be a comparison (either <= or >=) that incorporates =
                    -- so that it doesnt cut off all number repetitions
    