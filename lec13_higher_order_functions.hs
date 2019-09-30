import Debug.Trace

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

positives :: [Int] -> [Int]
positives [] = []
positives (x:xs) = 
    if x > 0
    then x:(positives xs) 
    else positives xs

upperCase :: String -> String
upperCase "" = ""
upperCase (c:cs) = 
    if c `elem` ['A'..'Z']
    then c:(upperCase cs)
    else upperCase cs

evens :: [Int] -> [Int]
evens [] = []
evens (x:xs) =
    if x `mod` 2 == 0
    then x:(evens xs)
    else evens xs

onlyThe p [] = []
onlyThe p (x:xs) =
    if p x
    then x:(onlyThe p xs)
    else onlyThe p xs

singletons lst = map aux lst
    where aux x = [x]

evens2 lst = filter aux lst
  where aux x = (x `mod` 2) == 0

evens3 lst = filter (\x -> (x `mod` 2) == 0) lst

multPairs :: [Int] -> [Int] -> [Int]
multPairs (x:xs) (y:ys) = (x*y):(multPairs xs ys) 
multPairs [] [] = []

concatPairs :: [String] -> [String] -> [String]
concatPairs (x:xs) (y:ys) = (x++y):(concatPairs xs ys) 
concatPairs [] [] = []

doToPairs :: (a -> b -> c) -> [a] -> [b] -> [c]
doToPairs f [] [] = []
doToPairs f [] allYs = []
doToPairs f allXs [] =[]
doToPairs f (x:xs) (y:ys) = (f x y):(doToPairs f xs ys)

catamari :: (a->b->b) -> [a] -> b -> b
catamari f b [] = traceShowId b
catamari f b (x:xs) = traceShowId $ f x (catamari f b xs)

multPositives :: [Int] -> Int
--with a list comprehension and built-in functions
--with recursion
--with a filter and fold
--just with a fold

myElem :: Eq a => a -> [a] -> Bool
--with a filter and built-in functions
--with recursion
--with a fold
