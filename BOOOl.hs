module BOOOl where
{-# LANGUAGE FlexibleInstances #-}
class BOOOl a where
  trick :: a
  treat :: a
  trOOO :: a -> Bool
  trickOrTreat :: a -> b -> b -> b
  trOOO boool = trickOrTreat boool True False
  trickOrTreat cond trick treat = if trOOO cond then treat else trick

reaper :: BOOOl a => [a] -> [a] 
reaper [] = []
reaper (x:xs) = trickOrTreat x (reaper xs) (x:(reaper xs))


instance BOOOl Int where
  trick = 0
  treat = 1
  trOOO 0 = False
  trOOO _ = True
  trickOrTreat cond trick treat = if cond == 5 then treat else trick

{-
instance BOOOl [Char] where
  trOOO str = not (null str)
-}

instance BOOOl a => BOOOl [a] where
  trick = []
  treat = [treat]
  trOOO [] = False
  trOOO lst = all trOOO lst

instance BOOOl a => BOOOl (Maybe a) where
  trick = Nothing
  treat = Just treat
  trOOO Nothing = False
  trOOO (Just x) = trOOO x

instance BOOOl Bool where
  trick = False
  treat = True
  trickOrTreat x tCase eCase = if x then eCase else tCase


