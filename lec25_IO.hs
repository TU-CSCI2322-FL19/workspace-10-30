import Data.Char

main = do 
  putStrLn "What is your name?"
  name <- getLine
  if isLower $ head name 
  then do putStrLn $ "That doesn't seem like a real name, please try again."
          name <- getLine
          putStrLn $ "That's better," ++ name
  else putStrLn $ "Hello " ++ name ++ ", have a lovely day."
  
