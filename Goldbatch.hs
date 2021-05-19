satisfyGoldbach :: Int -> Bool
satisfyGoldbach n = even n && isMoreThanTwo n && primeSatisfyTheSum n 2

isMoreThanTwo :: Int -> Bool
isMoreThanTwo n = n > 2

primeSatisfyTheSum :: Int -> Int -> Bool
primeSatisfyTheSum n k
  | n == k = False
  | isPrime k && isPrime (n - k) = True
  | otherwise = primeSatisfyTheSum n (k + 1)

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = n == minimumDiv n

minimumDiv :: Int -> Int
minimumDiv n = divSearch n 2

divSearch :: Int -> Int -> Int
divSearch n k
  | n `mod` k == 0 = k
  | otherwise = divSearch n (k + 1)

verifyConjectureTo :: Int -> Bool
verifyConjectureTo 4 = True
verifyConjectureTo n = satisfyGoldbach n && verifyConjectureTo (n - 2)

primeDescomposition :: Int -> (Int, Int)
primeDescomposition n
  | n <= 2 = undefined
  | otherwise = (primeToSum n 2, n - primeToSum n 2)

descompositionNumber :: Int -> Int
descompositionNumber n = descompositionCounter n 2 0

descompositionCounter :: Int -> Int -> Int -> Int
descompositionCounter n k i
  | n == k = i
  | k == primeToSum n k = descompositionCounter n (k + 1) (i + 1)
  | otherwise = descompositionCounter n (k + 1) i

primeToSum :: Int -> Int -> Int
primeToSum n k
  | n == k = n
  | isPrime k && isPrime (n - k) = k
  | otherwise = primeToSum n (k + 1)