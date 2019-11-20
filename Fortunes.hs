import Data.Char
import Debug.Trace
import System.Environment
import System.Console.GetOpt
import Text.Read

-- --name name -n name to define the name. If not defined, ask for it.
-- -k <k> print out k fortunes
-- --help -h -q
-- something (tbd) for non-interactive mode

data Flag = Help | Name String | NonInteractive | Count String deriving (Eq, Show)

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help)           "Print usage information and exit."
          , Option ['n'] ["name"] (ReqArg Name "<name>") "Define the user's name."
          , Option ['k'] []       (ReqArg Count "<num>") "Print out <num> fortunes."
          ]

prompt :: String -> IO String
prompt message = 
  do putStr message
     getLine

main :: IO ()
main = do
  args <- getArgs
  let (flags, inputs, errors) = getOpt Permute options args
  putStrLn (show flags)
  if (Help `elem` flags) || (not $ null errors) || length inputs > 1
  then putStrLn $ usageInfo "Usage: fortunes [options] [file]" options
  else do
    let filename = if null inputs then "fortunes.txt" else head inputs
    bigFortune <- readFile filename
    let fortunes = lines bigFortune
    name <- getName flags
    case getCount flags of
      Nothing -> giveFortune fortunes name
      Just k -> giveKFortunes fortunes name k

getName :: [Flag] -> IO String
getName ((Name s):_) = return s
getName (_:flags) = getName flags
getName [] = prompt "Please enter your name: "

getCount :: [Flag] -> Maybe Int
getCount ((Count s):_) = readMaybe s
getCount (_:flags) = getCount flags
getCount [] = Nothing

giveKFortunes :: [String] -> String -> Int -> IO ()
giveKFortunes fortunes name count =
  do let chosenFortunes = fortunesLookup name fortunes count
     sequence (map putStrLn chosenFortunes)
     return ()



giveFortune :: [String] -> String -> IO ()
giveFortune fortunes name = aux 0
  where aux i = do putStrLn $ "Here is your fortune, " ++ name ++ "."
                   putStrLn $ "\t" ++ (fortuneLookup name fortunes i)
                   answer <- prompt "Would you like another fortune? "
                   if map toLower answer `elem` ["y","yes","sure"]
                   then aux (i+1)
                   else return ()


fortuneLookup :: String -> [String] -> Int -> String
fortuneLookup name fortunes i =
  let index = sum [ord c | c <- name] `mod` length fortunes
  in fortunes !! (index+i)

fortunesLookup :: String -> [String] -> Int -> [String]
fortunesLookup name fortunes count =
  let index = sum [ord c | c <- name] `mod` length fortunes
  in take count (drop index fortunes)
