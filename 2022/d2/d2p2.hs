import Data.Functor ((<$>))
import Data.List (sortBy)
import System.Environment (getArgs)
import System.IO (readFile)

getInt m
  | m == "A" || m == "X" = 0
  | m == "B" || m == "Y" = 1
  | m == "C" || m == "Z" = 2
  | otherwise = 0

getOffset m
  | m == "X" = 2
  | m == "Y" = 0
  | m == "Z" = 1
  | otherwise = 0

getScore "" = 0
getScore g = 3 * getInt u
  where
    u = last . words $ g

getShape "" = 0
getShape u = 1 + getInt u

getMove g = head . drop (getInt o + getOffset p) $ c
  where
    o = head . words $ g
    p = last . words $ g
    c = cycle ["A", "B", "C"]

main = do
  (name : _) <- getArgs
  results <- lines <$> readFile name
  print . sum $ [getScore g + (getShape . getMove $ g) | g <- results]