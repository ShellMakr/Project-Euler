{--
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
--}

import Debug.Trace

divlist :: [Integer]
divlist = [1..20]


div_by_list :: [Integer] -> Integer -> Bool
div_by_list []  a       = True
div_by_list (x:xs)  a
 | a `mod` x == 0       = div_by_list xs a
 | otherwise            = False

twenty_div :: Integer -> Integer
twenty_div a 
 | a == 0               =  twenty_div (a+20)
 | a `mod` 20 == 0      = if div_by_list divlist a
                          then a
                          else twenty_div (a+20)
 | otherwise            = twenty_div (a+20)
 
main :: IO()
main = do
    print "smallest positive number that is evenly divisible by all of the numbers from 1 to 20 is"
    let result = twenty_div 0
    print $ show result
