union :: [a] -> [a] -> [a]
union xs ys = xs ++ ys

intersection :: Eq a => [a] -> [a] -> [a]
intersection xs ys = [x | x <- xs, x `elem` ys]

crossProduct :: [a] -> [b] -> [(a,b)]
crossProduct xs ys = [(x,y) | x <- xs, y <- ys]


setDifference :: Eq a => [a] -> [a] -> [a]
setDifference xs ys = [x | x <- xs, not (x `elem` ys)]

subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = null (setDifference xs ys) 
            -- [] == [x | x <- xs, not (x `elem` ys)]

powerSet :: Ord a => [a] -> [[a]]
--powerSet xs = [ys | ys `subset` xs]
powerSet xs = [[]] ++ [ [x] | x <- xs] ++ [ [x,y] | x <- xs, y <- xs, x < y] 
              ++ [ [x,y,z] | x <- xs, y <- xs, z <- xs, x < y, y < z]

fib :: Integer -> Integer
fib 1 = 1
fib 0 =1
fib n = fib (n-1) + fib (n-2)

myLength (x:xs) = 1 + myLength xs
myLength [] = 0

mySum [] = 0
mySum (x:xs) = let sumOfXs = mySum xs
               in x + sumOfXs

prodEvens [] = 1
prodEvens (x:xs) =
          let pexs = prodEvens xs
          in if even x then x *pexs else pexs

prodEvens2 [] = 1
prodEvens2 (x:xs) = if even x 
                    then x * (prodEvens2 xs) 
                    else prodEvens2 xs

prodEvens3 [] = 1
prodEvens3 (x:xs) 
    | even x    = x * (prodEvens3 xs) 
    | otherwise = prodEvens3 xs

myMaximum :: Ord a => [a] -> a
myMaximum [] = error "myMaximum called on an empty list."
myMaximum [x] = x
myMaximum (x:xs) =
      let maxXs = myMaximum xs
      in if x > maxXs 
         then x 
         else maxXs 
myMaximum2 [] = error "Can't do this. Why. Whyyyyy."
myMaximum2 [x] = x
myMaximum2 (x:xs)
  | x > maxXs = x
  | otherwise = maxXs
  where maxXs = myMaximum2 xs
