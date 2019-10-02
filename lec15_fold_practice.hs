prodList lst = foldr (\x acc -> x*acc) 1 lst
prodList2 lst = foldr (*) 1 lst --an excellent simplification
prodList3 :: [Integer] -> Integer
prodList3 = foldr (*) 1  --a simplification too far

myMaximum [] = error "AAAH"
myMaximum (x:xs) = foldr (\x acc -> max x acc) x xs
myMaximum (x:xs) = foldr max x xs --equivalent

myMaximum2 lst = foldr1 max lst
