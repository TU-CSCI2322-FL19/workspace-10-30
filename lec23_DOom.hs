safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead [] = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x:xs) = Just xs
safeTail [] = Nothing

safeDivide :: Double -> Double -> Maybe Double
safeDivide num 0 = Nothing
safeDivide num den = Just $ num / den

lst = [7,5]

firstOverSecond :: Maybe Double
firstOverSecond = 
  case safeHead lst of
    Nothing -> Nothing
    Just elem -> case safeTail lst of
                    Nothing -> Nothing
                    Just tl -> case safeHead tl of
                                  Nothing -> Nothing
                                  Just elem2 -> let prod = elem * elem2 
                                  --changed to match firstTimesSEcondOverTwo
                                                in safeDivide prod 2
        
firstOverSecondDANGER = (head lst) / (head (tail lst))

firstOverSecondMonad :: Maybe Double
firstOverSecondMonad = 
    do elem <- safeHead lst
       tl <- safeTail lst
       elem2 <- safeHead tl
       if elem == 1 then Nothing else Just ()
       safeDivide elem elem2

findFourth lst =
  do hd <- safeHead lst
     if hd == 0 then Nothing else Just ()
     tl <- safeTail lst
     tl2 <- safeTail tl
     tl3 <- safeTail tl2
     safeHead tl3

firstTimesSecondOverTwo =
  do elem <- safeHead lst
     tl <- safeTail lst
     elem2 <- safeHead tl
     let prod = (elem * elem2)
     return prod
     --safeDivide prod 2

lstA = [7,3,1,5]
lstB = [10,5,1]

allProducts = do
  a <- lstA
  b <- lstB
  return (a * b)

crossProduct ma mb = 
  do a <- ma
     b <- mb
     return (a,b)
