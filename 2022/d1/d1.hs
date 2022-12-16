import Data.Functor ((<$>))
import Data.List (sortBy)
import System.Environment (getArgs)
import System.IO (readFile)

getCounts :: [String] -> [Int]
getCounts [] = [0]
getCounts ("" : xs) = 0 : getCounts xs
getCounts (x : xs) = (read x + head (getCounts xs)) : tail (getCounts xs)

main = do
  (name : _) <- getArgs
  results <- lines <$> readFile name
  print . maximum . getCounts $ results
  print . sum $ take 3 (sortBy (flip compare) (getCounts results))