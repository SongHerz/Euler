{-
 - To check if an integer is a prime, we only have to check 
 - if the integer can be divided by all primes smaller than the square root of the integer.
 - Because any integer would be checked, number of primes is not determined,
 - and the prime list increases dynamically at run time.
 -}
isPrime :: [Integer] -> Integer -> Bool
isPrime primeList x = all (\y -> x `mod` y /= 0) factor_candi
    where factor_candi = takeWhile ( \y -> y <= x_cell) primeList
          x_cell = floor.sqrt.fromIntegral $ x

primeList :: [Integer]
primeList = 2: filter (\x -> isPrime primeList x) [3,5..]

{- This is the initial version before reading thread 3.
maxPrimeFactor :: Integer -> Integer
maxPrimeFactor x = if (sqrt.fromIntegral $ x) <= fromIntegral min_prime_factor
                     then min_prime_factor
                     else max min_prime_factor (maxPrimeFactor $ x `div` min_prime_factor)
    where min_prime_factor = head $ dropWhile (\prime -> x `mod` prime /= 0) primeList
-}

{-
 - The minimal factor of a composite must be a prime, and our prime list is in ascending order.
 - Later prime factor must be not less than older ones, and we only have to extract the last prime factor.
 -}
i_primeFactorList :: [Integer] -> Integer -> [Integer]
i_primeFactorList primeList x = if x == min_prime_factor
                     then [min_prime_factor]
                     else min_prime_factor : (i_primeFactorList new_prime_list (x `div` min_prime_factor))
    where new_prime_list = dropWhile (\prime -> x `mod` prime /= 0) primeList 
          min_prime_factor = head $ new_prime_list


primeFactorList x = i_primeFactorList primeList x

maxPrimeFactor x = last $ primeFactorList x


main = do
    print $ primeFactorList 600851475143
    print $ maxPrimeFactor 600851475143
