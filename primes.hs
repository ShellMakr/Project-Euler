module Primes where

import Debug.Trace

divlist :: (Num a) => [a]
divlist = [2,3,5,7]

-- checks if number is divisible by memebers of a list
isdiv :: (Num a, Integral a) => [a] -> a -> Bool
isdiv [] b = False
isdiv (x:xs) b = if b `mod` x == 0 then True
                else isdiv xs b

-- checks if number is divisible by the numbers in divlist
checkdiv :: (Num a, Integral a) => a -> Bool
checkdiv a = isdiv divlist a

-- checks to see if number is prime
-- based on the sieve of eukaryotes
isprime :: [Integer] -> Integer -> Integer -> Bool
isprime a b c
 | b == 1                                 = isprime a (b+1) b 
 | b == c                                 = True
 | isdiv a c == True && not (c `elem` a)  = False
 | isdiv a b == True                      = isprime a (b+1) c
 | isdiv a b == False                     = isprime (b : a) (b+1) c
 | otherwise             = if c `mod` b == 0 
                           then False                           
                           else isprime a (b+1) c

-- uses isprime to check for primes
checkprime :: Integer -> Bool
checkprime a = isprime [] 2 a
