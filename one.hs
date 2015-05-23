{--

Multiples of 3 and 5
Problem 1
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
--}

generate_multiples :: Int -> Int -> Int -> [Int]
generate_multiples a b c 
 | a == 0           = []
 | a `mod` b == 0   = a : generate_multiples (a-1) b c
 | a `mod` c == 0   = a : generate_multiples (a-1) b c
 | otherwise        = generate_multiples (a-1) b c

add_multiples :: Int -> Int 
add_multiples a = foldl (+) 0 $ generate_multiples a 3 5

-- main function
main :: IO()
main = do
    let result = add_multiples 999
    print "the sum of the multiples of 1000 is" 
    putStrLn $ show result

