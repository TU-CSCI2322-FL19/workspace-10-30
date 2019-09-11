import Debug.Trace
occurancesOfHead :: Eq a => [a] -> Int
occurancesOfHead [] = error "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAH"
occurancesOfHead (x:xs) = count x (x:xs)
  where count :: Eq a => a -> [a] -> Int
        count y [] = 0
        count y (x:xs) 
          | x == y    = 1 + count y xs
          | otherwise = count y xs

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = (rev xs) ++ [x]

frev lst = 
  let aux [] acc = traceShow ([] :: [Integer], acc) acc
      aux (x:xs) acc =  traceShow (x,xs,acc) $ aux xs (x:acc)
  in aux lst []

{-
gap :: [Int] -> Int
gap lst = maximum lst - minimum lst
-}

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

gap :: [Int] -> Int
gap [] = error "AAH"
gap lst  = 
    let aux l s []  = l - s
        aux l s (x:xs) -- aux (max x l) (min x s) xs  
          | x < s     = aux l x xs
          | x > l     = aux x s xs
          | otherwise = aux l s xs
    in aux (head lst) (head lst) lst

gap2 :: [Int] -> Int
gap2 [] = error "aaah"
gap2 lst = 
    let (largest, smallest) = range lst
    in largest - smallest

range :: [Int] -> (Int, Int)
range [x] = (x,x)
range (x:xs) = 
      let (l,s) = range xs
      in if x > l 
         then (x,s) 
         else if x < s 
              then (l,x) 
              else (l,s)


hoursToSchedule :: [(Int, Int)] -> Int
hoursToSchedule [] = 0
hoursToSchedule tasks = -- (t:ts) = let (h,m) = t 
    let (h,m) = sumTimes tasks
    in if m > 0 then h+1 else h

sumTimes :: [(Int, Int)] -> (Int, Int)
sumTimes [] = (0,0)
sumTimes ((h,m):ts) = 
    let (hts, mts) = sumTimes ts
        hTotal = h+hts
        mTotal = m + mts
    in (hTotal + mTotal `div` 60, mTotal `mod` 60)
