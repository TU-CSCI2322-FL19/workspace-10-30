import Data.Char
--Problems 0-2 will be done as a class.
--  0) Stop reading in the file every time.
--  1) Make an action to prompt the user and get a response.
--
--Core Exercise: Problems 2-4
--  2) Ask their name once. Remember it and print it out each time. Ask for a number for the
--  fortune.
--  3) Make an action getBool :: IO Bool to get a Yes/No response. 
--  Accept yes/y no/n in any captilization. If they input anything else, ask again.

--Extra Fun Problems: Problems 4-7
--  4) Feature creep! Ask the user what they want, and support the following operations. 
--  Get each one working before you move on to the next.
--  Hint: Make an "Request" data-type and a parseRequest:: String -> Request function. 
--   a) "Give me a fortune" (or any string with fortune in it)
--      Ask for a number, and print out the corresponding fortune
--   b) "Remember _______" 
--      Remember whatever comes after the command, and print it out on every prompt
--   c) "Remind me" 
--      Only print out the string when told to remind them
--  5) Make parseRequest more robust: return a Maybe Request. 
--  6) Add an action "What is _____" that evaluates a prefix mathematical expression and prints it
--out. You'll need to load Calc.hs.
--  7) Stop asking for a number: just give them the next fortune in the list.


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
