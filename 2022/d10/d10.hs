import Data.Functor ((<$>))
import Data.List (intercalate)
import Debug.Trace (trace)
import System.Environment (getArgs)
import System.IO (readFile)

data MS = MS {cy :: Int, x :: Int} deriving (Show)

data Inst
  = Noop
  | AddX Int
  deriving (Show)

applyInst s Noop = MS {cy = cy s + 1, x = x s}
applyInst s (AddX v) = MS {cy = cy s + 2, x = x s + v}

parseInsts [] = []
parseInsts ("" : ls) = parseInsts ls
parseInsts (l : ls)
  | head w == "noop" = Noop : parseInsts ls
  | head w == "addx" = AddX (read (last w)) : parseInsts ls
  where
    w = words l

atCycle s c = last . takeWhile ((< c) . cy) $ s

getSST s c = x s * c

draw s c
  | abs (x (atCycle s c) - mc) <= 1 = '#'
  | otherwise = '.'
  where
    mc = (c - 1) `mod` 40

getLines s l = map (draw s) [1 + 40 * l .. 40 + 40 * l]

main = do
  (name : _) <- getArgs
  results <- lines <$> readFile name
  let inst_stream = parseInsts results
  let reset = MS {cy = 0, x = 1}
  let states = scanl applyInst reset inst_stream
  let cys = [20, 60, 100, 140, 180, 220]
  let cys_states = map (atCycle states) cys
  let ssts = zipWith getSST cys_states cys
  print . sum $ ssts
  putStr . intercalate "\n" $ map (getLines states) [0 .. 5]
  putStr "\n"