import Data.Functor ((<$>))
import Data.List (group, sort)
import Debug.Trace (trace)
import System.Environment (getArgs)
import System.IO (readFile)

getDisp :: String -> [(Int, Int)]
getDisp l
  | d == "U" = replicate p (0, 1)
  | d == "D" = replicate p (0, -1)
  | d == "R" = replicate p (1, 0)
  | d == "L" = replicate p (-1, 0)
  where
    d = head . words $ l
    p = read . last . words $ l

diff (ax, ay) (bx, by) = (ax - bx, ay - by)

ndif a b = diff b a

summ (ax, ay) (bx, by) = (ax + bx, ay + by)

divv (ax, ay) n = (ax `div` n, ay `div` n)

dectz d
  | d > 0 = d - 1
  | d < 0 = d + 1

disp d
  | dx == 0 || dy == 0 = divv d 2
  | abs dx >= 2 && abs dy == abs dx - 1 = (dectz dx, dy)
  | abs dy >= 2 && abs dx == abs dy - 1 = (dx, dectz dy)
  | abs dx >= 2 && abs dx == abs dy = (dectz dx, dectz dy)
  where
    (dx, dy) = d

advance s
  | dx >= 2 || dy >= 2 || dx <= -2 || dy <= -2 = (h, summ t (disp d))
  | otherwise = s
  where
    (dx, dy) = uncurry diff s
    d = uncurry diff s
    h = fst s
    t = snd s

nextState (a, b) d = advance (summ a d, b)

pathToDeltas l = zipWith diff (tail l) (init l)

posToTail p = map snd (scanl nextState ((0, 0), (0, 0)) (pathToDeltas p))

main = do
  (name : _) <- getArgs
  results <- lines <$> readFile name
  let disps = map getDisp results
  let steps = foldl1 (++) disps
  let si = ((0, 0), (0, 0))
  let states = scanl nextState si steps
  let tails = map snd states
  let tailsort = sort tails
  print . length . group $ tailsort
  -- print (iterate posToTail (map fst states) !! 9)
  print . length . group . sort $ (iterate posToTail (map fst states) !! 9)