isPrime :: [Integer] -> Integer -> Bool
isPrime primeList x = ( all (\y -> x `mod` y /= 0) factor_candi)
    where factor_candi = takeWhile ( \y -> y <= x_cell) primeList; x_cell = toInteger.floor.sqrt.fromIntegral $ x

primeList :: [Integer]
primeList = 2: filter (\x -> isPrime primeList x) [3,5..]
