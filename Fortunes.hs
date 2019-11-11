import Data.Char
main :: IO ()
main = do
  bigFortune <- readFile "fortunes.txt"
  let fortunes = lines bigFortune
  putStr "Please enter your name: "
  name <- getLine
  putStrLn "Here is your fortune."
  putStrLn $ "\t" ++ (fortuneLookup name fortunes)
  putStr $ "Would you like another fortune? "
  answer <- getLine
  if map toLower answer `elem` ["y","yes","sure"]
  then main
  else return ()

fortuneLookup :: String -> [String] -> String
fortuneLookup name fortunes =
  let index = sum [ord c | c <- name] `mod` length fortunes
  in fortunes !! index
