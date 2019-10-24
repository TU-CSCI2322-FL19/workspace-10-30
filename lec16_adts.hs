data Contest = Rock | Scissor | Paper deriving (Show, Eq)

winner :: Contest -> Contest -> String
winner Rock Scissor = "Player One Wins!"
winner Rock Paper = "Player Two Wins!"
winner Scissor Paper = "Player One Wins!"
winner Scissor Rock = "Player Two Wins!"
winner Paper Rock = "Player One Wins!"
winner Paper Scissor = "Player Two Wins!"
winner _ _ = "Tie! Trie again!"

data Velocity = MPS Double | FPS Double deriving Eq

instance Show Velocity where
  show (MPS x) = show x ++ " m/s"
  show (FPS x) = show x ++ " f/s"


readVelocity :: String -> Velocity
readVelocity str = case words str of
    [x, "m/s"] -> MPS $ read x
    [y, "f/s"] -> FPS $ read y
    _ -> error "Invalid velocity."
inMPS :: Velocity -> Double
inMPS (MPS x) = x
inMPS (FPS y) = y / 3.28084 

type Point = (Double, Double)
data Shape = Circle {center :: Point, radius :: Double }
           | Rectangle { center :: Point, width :: Double, height :: Double}
           deriving (Show, Eq)

area :: Shape -> Double
area (Circle center r) = pi * r^2
area (Rectangle (x1,y1) w h ) = w * h

