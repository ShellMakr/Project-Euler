{--
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number? 
--}
import Primes

-- checks to see if number is prime
-- based on the sieve of eukaryotes
-- primes found -> incrimentor
primes_value :: Integer -> Integer -> Integer -> Integer
primes_value a b c
 | checkprime b == True    = if (a+1) == c
                          then b
                          else primes_value (a+1) (b+1) c
 | otherwise            =  primes_value a (b+1) c


main :: IO() 
main = do
    let result = primes_value 10001
    print "the value of the 10001 prime is "
    print $ show result