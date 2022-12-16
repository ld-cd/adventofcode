import Data.Array (Array, assocs, bounds, elems, indices, listArray, (!), (//))
import Data.Char (ord)
import Data.Functor ((<$>))
import Data.Graph (graphFromEdges)
import Data.Ix (Ix, inRange)
import Data.List (intercalate, minimumBy)
import Data.Set (Set, insert, notMember, singleton)
import Debug.Trace (trace)
import System.Environment (getArgs)
import System.IO (readFile)
import Text.Printf

c2h 'S' = c2h 'a'
c2h 'E' = c2h 'z'
c2h c = ord c - 97

addi (ax, ay) (bx, by) = (ax + bx, ay + by)

getCardinals p b = [p `addi` i | i <- [(0, 1), (0, -1), (1, 0), (-1, 0)], inRange b (addi p i)]

getNeighbors hs p = [i | i <- c, (hs ! i) - (hs ! p) <= 1]
  where
    c = getCardinals p . bounds $ hs

updateDists dp edges dists = dists // zip edges (map (min (dp + 1) . (dists !)) edges)

nextPoint :: Ix t => Array t Int -> Set t -> t
nextPoint dists visited = fst . minimumBy (\a b -> compare (snd a) (snd b)) $ candidates
  where
    candidates = filter (\a -> notMember (fst a) visited) (assocs dists)

dijkstra p edges dists visited target
  | target == p = dists
  | otherwise = dijkstra np edges nd nv target
  where
    dp = dists ! p
    ep = edges ! p
    nd = updateDists dp ep dists
    np = nextPoint nd visited
    nv = insert np visited

strLn :: [Int] -> String
strLn = concatMap (\x -> if x /= 1000 then printf "%3d" x else " -1")

strDists :: [Int] -> Int -> String
strDists [] _ = []
strDists dists w = (strLn . take w $ dists) ++ "\n" ++ strDists (drop w dists) w

inVecs i edges = [c | c <- indices edges, elem i $ edges ! c]

invertEdges :: Array (Int, Int) [(Int, Int)] -> Array (Int, Int) [(Int, Int)]
invertEdges edges = listArray (bounds edges) [inVecs i edges | i <- indices edges]

main = do
  (name : _) <- getArgs
  results <- lines <$> readFile name
  let w = length . head $ results
  let h = length results
  let b = ((0, 0), (h - 1, w - 1))
  let chars = listArray b . concat $ results
  let heights = listArray b . map c2h . concat $ results

  let s = fst . head . filter (\x -> snd x == 'S') . assocs $ chars
  let e = fst . head . filter (\x -> snd x == 'E') . assocs $ chars

  let dists = (listArray b . replicate (w * h) $ 1000) // [(s, 0)]

  let edgel = map (getNeighbors heights) . indices $ heights
  let edges = listArray b edgel

  let newdists = dijkstra s edges dists (singleton s) e
  putStr . strDists (elems heights) $ w
  putStr "\n"
  putStr . strDists (elems newdists) $ w
  print $ newdists ! e

  let invedges = invertEdges edges
  let invdists = dijkstra e invedges (dists // [(s, 1000), (e, 0)]) (singleton e) s
  let landingdists = [invdists ! i | i <- indices invdists, heights ! i == 0]

  putStr . strDists (elems invdists) $ w
  print . minimum $ landingdists