satisfyGoldbach :: Int -> Bool
satisfyGoldbach n = even n && isMoreThanTwo n && sumOfTwoPrimes n

isMoreThanTwo :: Int -> Bool
isMoreThanTwo n = n > 2

sumOfTwoPrimes :: Int -> Bool
sumOfTwoPrimes n = isPrime (primeSatisfyTheSum n 2)

primeSatisfyTheSum :: Int -> Int -> Int
primeSatisfyTheSum n k
  | n == k = n
  | isPrime k && isPrime (n - k) && k + (n - k) == n = k
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
verifyConjectureTo n = verifyEachEven n 4

verifyEachEven :: Int -> Int -> Bool
verifyEachEven n k
  | n < 4 = False
  | odd n = False
  | n == k = True
  | satisfyGoldbach k = verifyEachEven n (k + 2)
  | otherwise = False

primeDescomposition :: Int -> (Int, Int)
primeDescomposition n
  | n <= 2 = undefined
  | otherwise = (primeSatisfyTheSum n 2, n - primeSatisfyTheSum n 2)

descompositionNumber :: Int -> Int
descompositionNumber n = descompositionCounter n 2 0

descompositionCounter :: Int -> Int -> Int -> Int
descompositionCounter n k i
  | n == k = i
  | k == primeSatisfyTheSum n k = descompositionCounter n (k + 1) (i + 1)
  | otherwise = descompositionCounter n (k + 1) i