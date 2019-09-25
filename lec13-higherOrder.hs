add4 x = x + 4
add8 x = x + 8

addY y x = x + y


add8All [] = []
add8All (x:xs) = (x+8):(add8All xs)

add10All [] = []
add10All (x:xs) = (x+10):(add10All xs)

addYAll y [] = []
addYAll y (x:xs) = (x+y):(addYAll y xs)

absAll [] = []
absAll (x:xs) = (abs x):(absAll xs)

doubleAll [] = []
doubleAll (x:xs) = (2*x):(doubleAll xs)

doToAll f [] = []
doToAll f (x:xs) = (f x):(doToAll f xs)

dAdd :: Int -> (Int -> Int)
dAdd x y = 2 * x + y
