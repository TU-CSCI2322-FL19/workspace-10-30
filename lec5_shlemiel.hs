lst = [1..10000]
x = 20
i = length lst
k = sum [sum (lst ++ [i]) | i <- lst]
k = sum [sum (i:lst) | i <- lst]

myHead :: [a] -> a
myHead (x:xs) = x
myHead [] = error "myHead called on an empty list."
