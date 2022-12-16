import Data.Char (isUpper, ord)
import Data.Functor ((<$>))
import Data.Text (last, pack, singleton, splitOn, unpack)
import System.Environment (getArgs)
import System.IO (readFile)

parseHeader l
  | h == "   " && lt >= 3 = [] : parseHeader (tail t)
  | h == "   " = [[]]
  | lt >= 3 = [l !! 1] : parseHeader (tail t)
  | otherwise = [[l !! 1]]
  where
    h = take 3 l
    t = drop 3 l
    lt = length l - 3

stack = zipWith (++)

parseStacks ls
  | head nl == "1" = tl
  | otherwise = tl `stack` parseStacks (tail ls)
  where
    tl = parseHeader . head $ ls
    nl = parseHeader (ls !! 1)

getMoves ls
  | head ls == "" = tail ls
  | otherwise = getMoves . tail $ ls

parseMove :: String -> (Int, Int, Int)
parseMove "" = (0, 0, 0)
parseMove l =
  (read (w !! 1), -1 + read (w !! 3), -1 + read (w !! 5))
  where
    w = words l

applyMovePar state (c, f, t) i
  | i == f = drop c (state !! i)
  | i == t = reverse (take c (state !! f)) ++ (state !! i)
  | otherwise = state !! i

applyMove state m = [applyMovePar state m i | i <- [0 .. length state - 1]]

main = do
  (name : _) <- getArgs
  results <- lines <$> readFile name
  let stack = parseStacks results
  let moves = map parseMove (drop 1 (dropWhile (/= "") results))
  let endState = foldl applyMove stack moves
  print moves
  print stack
  print endState
  print [head s | s <- endState]