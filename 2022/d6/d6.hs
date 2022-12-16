import Data.Functor ((<$>))
import Data.List (intersect)
import System.Environment (getArgs)
import System.IO (readFile)

distinct [] = True
distinct l = null ([head l] `intersect` tail l) && (distinct . tail $ l)

fNum :: Int -> String -> Int
fNum f [] = f
fNum f l
  | distinct (take f l) = f
  | otherwise = 1 + fNum f (tail l)

sopNum = fNum 4

msgNum = fNum 14

main = do
  (name : _) <- getArgs
  results <- lines <$> readFile name
  print . sopNum $ "bvwbjplbgvbhsrlpgdmjqwftvncz"
  print . sopNum $ (head results)
  print . msgNum $ (head results)