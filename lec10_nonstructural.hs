import Debug.Trace
quickSort [] = []
quickSort (pivot:xs) = 
  let earliers = [y | y <- xs, y <= pivot]
      laters   = [y | y <- xs, y > pivot]
      sortedEarliers = quickSort earliers
      sortedLaters = quickSort laters
  in sortedEarliers ++ [pivot] ++ sortedLaters 



hanoi :: Int -> Char -> Char -> Char -> [(Int, Char, Char)]
hanoi 0 str mid end = []
hanoi n str mid end  = 
  let firstBatch = hanoi (n-1) str end mid
      middleMove = (n, str, end)
      lastBatch = hanoi (n-1) mid str end
  in firstBatch ++ [middleMove] ++ lastBatch
