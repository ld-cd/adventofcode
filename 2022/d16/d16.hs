import Data.Functor ((<$>))
import Data.Graph (Graph, graphFromEdges, vertices)
import Data.List (intercalate, minimumBy, splitAt)
import Data.Map.Strict qualified as M
import Data.Set (Set, elemAt, elems, empty, fromAscList, fromList, insert, intersection, notMember, null, powerSet, singleton, size, union, (\\))
import Data.Tuple (swap)
import Debug.Trace (trace)
import System.Environment (getArgs)
import System.IO (readFile)

parseList :: [String] -> [String]
parseList = map $ takeWhile (/= ',')

parseRate :: String -> Int
parseRate = read . init . drop 5

parseLine l = (parseRate $ ws !! 4, ws !! 1, parseList . drop 9 $ ws) where ws = words l

getOrBase (nfv, vfk) = maybe (0, "AA", []) nfv . vfk

multiBFS :: (Graph, a -> (Int, String, [String]), String -> Maybe a) -> Set String -> Set String -> Set String -> [(String, Int)]
multiBFS (g, nfv, vfk) openedSet searchSet targetSet
  | Data.Set.null targetSet || Data.Set.null searchSet = []
  | otherwise =
      (map (,1) . elems $ found)
        ++ (map (\(a, d) -> (a, d + 1)) . multiBFS (g, nfv, vfk) (edges `union` openedSet) unsearched $ unfound)
  where
    edges = fromList . concat $ [(\(_, _, ed) -> ed) . getOrBase (nfv, vfk) $ i | i <- elems searchSet]
    unsearched = edges \\ openedSet
    found = edges `intersection` targetSet
    unfound = targetSet \\ found

getEdges graph tos from = map (\(t, d) -> ((from, t), d)) . multiBFS graph (singleton from) (singleton from) $ (tos \\ singleton "AA")

maximizeFlow' :: M.Map (String, String) Int -> M.Map String Int -> (Int, String) -> Int
maximizeFlow' edgeMap flowMap (rt, cpos)
  | M.null remainMap = (rt - 1) * (validMap M.! cpos)
  | otherwise = (rt - 1) * (validMap M.! cpos) + maximum (map (maximizeFlow' edgeMap remainMap) nexts)
  where
    remSet = M.keysSet flowMap \\ singleton cpos
    illegal = filter (\x -> rt - (1 + edgeMap M.! (cpos, x)) < 2) . elems $ remSet
    illegalSet = fromAscList illegal
    filterSet = insert cpos illegalSet
    validMap = flowMap `M.withoutKeys` illegalSet
    remainMap = M.delete cpos validMap
    nexts = map (\x -> (rt - (1 + edgeMap M.! (cpos, x)), x)) . elems $ remSet \\ illegalSet

maximizeFlow valveDists edgeMap flowMap rt =
  maximum . map (\(v, d) -> maximizeFlow' edgeMap flowMap (rt - d, v)) $ valveDists

generatePairings :: Ord a => Set (Set a) -> Set a -> [(Set a, Set a)]
generatePairings combs total
  | size combs == 0 = []
  | size m == 0 || size r == 0 = generatePairings rest total
  | otherwise = (m, r) : generatePairings rest total
  where
    m = elemAt 0 combs
    r = total \\ m
    rest = combs \\ fromAscList [m, r]

maximizeHumphsterFlow valveDists edgeMap flowMap rt (yourNodes, humphNodes) =
  maximizeFlow (filter ((`notMember` humphNodes) . fst) valveDists) edgeMap (flowMap `M.withoutKeys` humphNodes) rt
    + maximizeFlow (filter ((`notMember` yourNodes) . fst) valveDists) edgeMap (flowMap `M.withoutKeys` yourNodes) rt

main = do
  (name : _) <- getArgs
  results <- lines <$> readFile name
  let graph = graphFromEdges . map parseLine $ results
  let (gm, nfv, vfk) = graph
  -- let zverts = filter ((==) 0 . fst) [(\(i, x, _) -> (i, x)) i | i <- map parseLine results]
  let nzverts = filter ((/=) 0 . fst) [(\(i, x, _) -> (i, x)) i | i <- map parseLine results]
  -- let zv = map snd zverts
  let nv = map snd nzverts
  let nvs = fromList nv
  let nvd = concatMap (getEdges graph nvs) nv
  let valveDists = map (\((_, v), d) -> (v, d)) $ getEdges graph nvs "AA"
  let edgeMap = M.fromList nvd
  let flowMap = M.fromList . map swap $ nzverts

  print . maximizeFlow valveDists edgeMap flowMap $ 30

  let combs = powerSet nvs
  let pairings = generatePairings combs nvs
  print . maximum . map (maximizeHumphsterFlow valveDists edgeMap flowMap 26) $ pairings