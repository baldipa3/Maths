times :: [Int] -> Int
times [] = 1
times l = head l * times (tail l)

sumN :: Int -> [Int] -> [Int]
sumN n [] = []
sumN n l = head l + n : sumN n (tail l)

sumFirst :: [Int] -> [Int]
sumFirst [] = []
sumFirst l = sumN (head l) l

sumLast :: [Int] -> [Int]
sumLast [] = []
sumLast l = reverse (sumFirst (reverse l))

--evens :: [Int] -> [Int]
--evens [] = []
--evens (x : xs)
--  | even x = (x : [])
--  | otherwise = evens xs
