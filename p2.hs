isPrime :: [Integer] -> Integer -> Bool
isPrime primeList x = ( all (\y -> x `mod` y /= 0) factor_candi)
    where factor_candi = takeWhile ( \y -> y <= x_cell) primeList
          x_cell = toInteger.floor.sqrt.fromIntegral $ x

primeList :: [Integer]
primeList = 2: filter (\x -> isPrime primeList x) [3,5..]

maxPrimeFactor :: Integer -> Integer
maxPrimeFactor x = if (sqrt.fromIntegral $ x) <= fromIntegral min_prime_factor
                     then min_prime_factor
                     else max min_prime_factor (maxPrimeFactor $ x `div` min_prime_factor)
    where min_prime_factor = head $ dropWhile (\prime -> x `mod` prime /= 0) primeList
