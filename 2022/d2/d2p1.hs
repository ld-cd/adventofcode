import Data.Functor ((<$>))
import Data.List (sortBy)
import System.Environment (getArgs)
import System.IO (readFile)

getScore "" = 0
getScore g
  | o == "A" && u == "X" = 3
  | o == "B" && u == "Y" = 3
  | o == "C" && u == "Z" = 3
  | o == "A" && u == "Y" = 6
  | o == "B" && u == "Z" = 6
  | o == "C" && u == "X" = 6
  | otherwise = 0
  where
    o = head . words $ g
    u = last . words $ g

getShape "" = 0
getShape g
  | u == "X" = 1
  | u == "Y" = 2
  | u == "Z" = 3
  | otherwise = 0
  where
    u = last . words $ g

main = do
  (name : _) <- getArgs
  results <- lines <$> readFile name
  print . sum $ [getScore g + getShape g | g <- results]