import Data.Functor ((<$>))
import Data.List (singleton)
import Language.Haskell.TH (match)
import System.Environment (getArgs)
import System.IO (readFile)

parseLine :: [Char] -> [Int]
parseLine l = [read [i] | i <- l]

transpose ([] : _) = []
transpose m = map head m : transpose (map tail m)

flips = map reverse

matmax = zipWith (zipWith (max))

runsub :: Int -> [Int] -> [Int]
runsub m [] = []
runsub m (l : ls) = (l - m) : runsub (max l m) ls

visible = map (runsub (0 - 1))

getSlices m r c =
  [ reverse (take (c + 1) (m !! r)),
    drop c (m !! r),
    reverse (take (r + 1) (transpose m !! c)),
    drop r (transpose m !! c)
  ]

getVD h [] = 0
getVD h (l : ls)
  | l < h = 1 + getVD h ls
  | otherwise = 1

viewScore :: [[Int]] -> Int
viewScore s = product (map (getVD (head . head $ s) . tail) s)

main = do
  (name : _) <- getArgs
  results <- lines <$> readFile name
  let mat = map parseLine results
  let left = visible mat
  let right = flips . visible . flips $ mat
  let top = transpose . visible . transpose $ mat
  let bottom = transpose . flips . visible . flips . transpose $ mat
  let total = left `matmax` right `matmax` top `matmax` bottom
  let vis = sum (map (sum . map (\a -> if a >= 1 then 1 else 0)) total)
  let width = length mat
  let height = length . head $ mat
  let vds = [viewScore (getSlices mat r c) | r <- [0 .. width - 1], c <- [0 .. height - 1]]
  print . maximum $ vds
  print vis