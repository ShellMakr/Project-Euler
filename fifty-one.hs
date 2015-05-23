{--
By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.

Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.
--}


import System.Environment
import Data.List
import Data.Char
import Primes( checkprime )
import Algorithms ( quicksort ) 

-- HELPER FUNCTIONS

-- creates a list of zeroes as long as specified
zero_list :: (Eq a, Num a) => a -> [a]
zero_list a 
 | a == 0       = []
 | otherwise    = 0 : zero_list (a-1)

-- ORDERING FUNCTIONS FUNCTIONS

-- cretaes a list of numbers where the duplicated elements are adjacent
-- passer -> non dups -> dups
move_elem_fowrd :: [a] -> [a] -> [a] -> [[a]]
move_elem_fowrd a [] c     = [ c ++ a]
move_elem_fowrd a (x:xs) c = 
    let new_order = [x : a ++ c ++ xs ] 
    in new_order ++ move_elem_fowrd (x : a) xs c 

-- gets all order combinations of elements in list
in_all_pos = move_elem_fowrd [] 

-- creates [[a]] where each [a] has space between the duplicates
-- preordered -> non_dups -> dups
csn :: (Num a) => [a] -> [a] -> [a] -> [[a]]
csn a [] []           = []
csn a (b:[]) (c:[])   = []
csn a b (c:[])        = []
csn a b []            = []
csn a [] c            = []
csn a (x:xs) (y:ys)   = 
    let new_order = y : x : a ++ ys
    in (in_all_pos xs new_order) ++ csn (y : x : a)  xs ys

-- renaming with empty starting preorder
spaced_reorder = csn []

--NOTE: reverse lists longer than 3 to get the something like 6066 to 6606

-- get all posssible values of the surrounding numbers (non-dupa)
apn :: (Num a, Eq a) => a -> [a] -> [a] -> [[a]]
apn a b []      = []
apn a b (x:xs)  = 
    if a == 9 then []
    else [b ++ [(x+1)] ++ xs] 
        ++ apn (x+1) b ((x+1):xs) ++ apn (x+1) (b++[(x+1)]) xs

-- plus one on all values
plusone :: (Num a) => [a] -> [a]
plusone []        = []
plusone (x:xs)    = (x+1) : plusone xs

-- all possible duplicate values 
all_pos_dups :: (Num a,  Eq a) => [a] -> [[a]]
all_pos_dups []  = []
all_pos_dups (x:xs) 
 | x == 9      = []
 | otherwise    =   
    let increased = plusone (x:xs) 
    in [increased] ++ all_pos_dups increased

-- pops head of list element and return lists of tails
-- list with useless head -> tail only
pop_head :: [[a]] -> [[a]]
pop_head []   = []
pop_head (x:xs) = (tail x) : pop_head xs

-- all possible combinations of non_dups
-- first is non dups, then dups 
all_non_dups :: [Int] -> [[Int]]
all_non_dups a = pop_head $ apn 0 [0] a

-- list of all possible duplicate values
apdr :: [a] -> [[a]] -> [[a]]
apdr a []       = []
apdr a (x:xs)   = in_all_pos a x ++ apdr a xs

-- with non duplcates create all order with duplicates
-- non-duplicates -> duplicate size
reorder_all_dups :: [Int] -> Int -> [[Int]]
reorder_all_dups a b = (apdr a $ all_pos_dups $ zero_list b) ++ spaced_reorder (zero_list b) a

--list of all possible values of a size
gen_all_mid_values :: [[Int]] -> Int -> [[Int]]
gen_all_mid_values [] a     = []
gen_all_mid_values (x:xs) a =  reorder_all_dups x a ++ gen_all_mid_values xs a

-- generates all values 
all_mid_values :: Int -> Int -> [[Int]] 
all_mid_values a b = gen_all_mid_values (all_non_dups $ zero_list a) b  

-- keeps only least signifcant digits that can be prime
mal :: (Eq a, Num a) => [[a]] -> [[a]]
mal []     = []
mal (x:xs) = 
    if ((head $ tail x) `elem` [1,3,7])
    then [x] ++ mal xs 
    else  mal xs

most_and_least_sigs = mal $ all_non_dups [0,0]

-- creates final list of list of numbers
num_list :: [a] -> [[a]] -> [[a]]
num_list a []           = []
num_list (x:xs) (y:ys)  = [x : y ++ [(head xs)]] ++ num_list (x:xs) ys



-- creates possible prime numbers of a particular length
-- most & least combo's -> list of mid values
--possible_primes :: [[Int]] -> [[Int]] -> [[[Int]]]
--possible_primes [] b         = []
--possible_primes (x:xs) b     = [num_list x b] ++ possible_primes xs b
{--

-- converts a list of lists of ints to a list of int
converter :: [[Int]] -> [Int]
converter []        = []
converter (x:xs)    = [foldl adder 0 x] ++ converter xs
                        where adder num d = 10*num + d

-- converts the nest list to list of numbers
convert :: [[[Int]]] -> [[Int]]
convert []     = []
convert (x:xs) = [converter x] ++ convert xs

--}

--check primes and print prime list properties


{-- 
-- takes integers for size of dups and non-dups
-- non-dups size -> dup size
to_test_list :: Int -> Int -> [[Int]]
to_test_list a b = possible_primes most_and_least_sigs $ all_mid_values a b

-- stores only values that add up to parameter
limiter :: (Ord a, Num a) => [[a]] -> a -> [[a]]
limiter [] b        = []
limiter (x:xs) b    = if sum x == b && (head x /= 0) 
                        && not (( head $ tail x) `elem` [0,1])
                      then x : limiter xs b
                      else limiter xs b

-- get duplicate and non duplicate combinations for mid place  vals
mid_place_prop :: Int -> [[Int]]
mid_place_prop a = limiter (all_non_dups [0,0]) a


-- removes all primes from list
list_prime_checker :: [Int] -> [Int]
list_prime_checker []     = []
list_prime_checker (x:xs) = 
    if checkprime x 
    then x : list_prime_checker xs
    else list_prime_checker xs




---------------------------------------------------------
-- generate possible primes based on number of dups nd non-dups combined
range_based_gen :: [[Int]] -> [[Int]]
range_based_gen []      = []
range_based_gen (x:xs)  = to_test_list (head x) (head $ tail x) ++ range_based_gen xs


-- converts int in list of list of int to list of int
converter :: [[Int]] -> [Int]
converter []        = []
converter (x:xs)    = [foldl adder 0 x] ++ converter xs
                        where adder num d = 10*num + d

-- compares the first and last elements to see who is greater
greater_tip_checker :: [Int] -> [Int] -> Bool
greater_tip_checker a b = 
            if ((head a * 10) + last a) >= ((head b * 10) + last b)
            then True
            else False        

-- compare the first and last element to see who is  lesser
lesser_tip_checker :: [Int] -> [Int] -> Bool
lesser_tip_checker a b = 
            if ((head a * 10) + last a) < ((head b * 10) + last b)
            then True
            else False        

-- create list with the same head and last element
sectioning :: [[Int]] -> [[Int]]
sectioning []   = []
sectioning (x:xs) = sectioning (filter (`lesser_tip_checker` x ) xs) ++ [x] ++ sectioning (filter ( `greater_tip_checker` x) xs)            
---------------------------------------------------------

--}



-- main function                        
main :: IO()
main = do
    args <- getArgs
    
    --let result = num_list 5 6 $ move_elem_fowrd [] 4 4 (zero_list 3) [4]
    let result = in_all_pos [0,0,0] [4,4]
    putStrLn $ show result