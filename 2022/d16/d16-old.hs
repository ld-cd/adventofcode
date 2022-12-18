import Data.Array (Array, assocs, bounds, elems, indices, listArray, (!), (//))
import Data.Functor ((<$>))
import Data.Graph (Graph, graphFromEdges, vertices)
import Data.List (intercalate, minimumBy, splitAt)
import Data.Set (Set, fromList, insert, notMember, singleton, size)
import Debug.Trace (trace)
import System.Environment (getArgs)
import System.IO (readFile)

parseList :: [String] -> [String]
parseList = map $ takeWhile (/= ',')

parseRate :: String -> Int
parseRate = read . init . drop 5

parseLine l = (parseRate $ ws !! 4, ws !! 1, parseList . drop 9 $ ws) where ws = words l

maxHelper edges rt graph openedSet bannedSet
  | null validEdges = 0
  | otherwise = maximum (map (\x -> maximizeFlow x rt graph openedSet bannedSet) validEdges)
  where
    validEdges = filter (`notMember` bannedSet) edges

maximizeFlow :: String -> Int -> (Graph, a -> (Int, String, [String]), String -> Maybe a) -> Set String -> Set String -> Int
maximizeFlow ck rt (g, nfv, vfk) openedSet bannedSet
  | rt <= 1 = 0
  | rt == 2 = fl
  | we = fl * (rt - 1)
  | nm = max (fl * (rt - 1) + maxHelper ed (rt - 2) (g, nfv, vfk) (insert ck openedSet) (singleton ck)) (maxHelper ed (rt - 1) (g, nfv, vfk) openedSet (insert ck bannedSet))
  | otherwise = maxHelper ed (rt - 1) (g, nfv, vfk) openedSet (insert ck bannedSet)
  where
    (fl, _, ed) = maybe (0, "AA", []) nfv $ vfk ck -- (trace (show ck ++ show rt) ck)
    nm = notMember ck openedSet
    we = nm && size openedSet == (length . vertices $ g) - 1

main = do
  (name : _) <- getArgs
  results <- lines <$> readFile name
  let graph = graphFromEdges . map parseLine $ results
  let (gm, nfv, vfk) = graph
  let zverts = filter ((==) 0 . fst) [(\(i, x, _) -> (i, x)) i | i <- map parseLine results]
  let zv = map snd zverts

  print (maximizeFlow "AA" 30 graph (fromList zv) (singleton "AA"))