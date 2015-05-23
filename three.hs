{--
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?

--}
import Primes

list_prime_factors :: Integer -> Integer -> [Integer]
list_prime_factors a b
 | a == 1           = list_prime_factors (a+1) b
 | a == b           = []
 | b `mod` a == 0   = if checkprime a
                      then a : list_prime_factors (a+1) b
                      else list_prime_factors (a+1) b
 | otherwise        = list_prime_factors (a+1) b


max_factor :: Integer -> Integer
max_factor a = maximum $ list_prime_factors 1 a


main :: IO() 
main = do
    let result = max_factor 600851475143
    print $ show result
