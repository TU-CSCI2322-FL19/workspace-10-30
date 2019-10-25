{-# LANGUAGE FlexibleInstances #-}
class BOOOl a where
  trOOO :: a -> Bool
  trickOrTreat :: a -> b -> b -> b
  trOOO boool = trickOrTreat boool True False
  trickOrTreat cond trick treat = if trOOO cond then treat else trick

instance BOOOl Int where
  trOOO 0 = False
  trOOO _ = True

instance BOOOl [Char] where
  trOOO str = not (null str)

instance BOOOl a => BOOOl [a] where
  trOOO [] = False
  trOOO lst = all trOOO lst

instance BOOOl a => BOOOl (Maybe a) where
  trOOO Nothing = False
  trOOO (Just x) = trOOO x

instance BOOOl Bool where
  trOOO x = x
