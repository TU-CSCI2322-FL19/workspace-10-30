increment :: Integer -> Integer
increment x = x + 1

f :: (Integer, Integer) -> Integer
f tup = 2 * (fst tup) + (snd tup)

addTriple :: (Integer, Integer, Integer) -> Integer
addTriple (x, y, z) = x + y + z

isEmpty (x:xs) = False
isEmpty [] = True

evens = [2,4..]

x = 9

isLucky 3 = True
isLucky 7 = True
isLucky x = False

cylinder r h = 
  let sideArea = 2 * pi * r * h
      topArea = pi * r^2
  in sideArea + 2 * topArea 

cylinder2 r h = sideArea + 2 * topArea
  where sideArea = 2 * pi * r * h
        topArea = pi * r^2
