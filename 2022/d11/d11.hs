import Data.Array.Base (nullStablePtr)
import Data.Functor ((<$>))
import Data.List (sort)
import Debug.Trace (trace)
import System.Environment (getArgs)
import System.IO (readFile)

data Monkey = Monkey {items :: [Int], op :: Int -> Int, test :: Int -> Bool, target :: (Int, Int)}

extractMonkey ls = (takeWhile (/= "") ls, tail . dropWhile (/= "") $ ls)

parseList :: [[Char]] -> [Int]
parseList = map (read . takeWhile (/= ','))

parseOpt ("*" : rs)
  | head rs == "old" = \x -> x * x
  | otherwise = \x -> x * (read . head $ rs)
parseOpt ("+" : rs)
  | head rs == "old" = \x -> x + x
  | otherwise = \x -> x + (read . head $ rs)

parseMonkey :: [String] -> Monkey
parseMonkey ls = Monkey {items = i, op = o, test = t, target = (t1, t2)}
  where
    i = parseList (drop 2 (words (ls !! 1)))
    o = parseOpt . drop 4 . words $ (ls !! 2)
    t x = x `mod` read (words (ls !! 3) !! 3) == 0
    t1 = read (words (ls !! 4) !! 5)
    t2 = read (words (ls !! 5) !! 5)

getMonkeys [] = []
getMonkeys ls = parseMonkey fls : getMonkeys nls
  where
    (fls, nls) = extractMonkey ls

getThrows m = (filter (test m) nl, filter ((== False) . test m) nl) where nl = map ((`div` 3) . op m) (items m)

replace ls n e = take n ls ++ [e] ++ drop (n + 1) ls

replaceItems m is = Monkey {items = is, op = op m, test = test m, target = target m}

appendItems m is = replaceItems m (items m ++ is)

runMonkey :: [Monkey] -> Int -> ([Monkey], [Int])
runMonkey ms mn = (replace (replace (replace ms tn (appendItems tm tt)) fn (appendItems fm ft)) mn (replaceItems m []), items m)
  where
    m = ms !! mn
    (tt, ft) = getThrows m
    (tn, fn) = target m
    (tm, fm) = (ms !! tn, ms !! fn)

runMonkeys ms [] = []
runMonkeys ms (mn : mns) = runMonkey ms mn : runMonkeys (fst (runMonkey ms mn)) mns

runTurn ms = runMonkeys ms [0 .. length ms - 1]

runTurns :: (Eq t, Num t) => [Monkey] -> t -> [[([Monkey], [Int])]]
runTurns ms 0 = []
runTurns ms n = runTurn ms : runTurns (fst . last . runTurn $ ms) (n - 1)

main = do
  (name : _) <- getArgs
  results <- lines <$> readFile name
  let monkeys = getMonkeys results
  let game = runTurns monkeys 20
  let throws = map (map snd) game
  let inspects = map (map length) throws
  let totals = foldl1 (zipWith (+)) inspects
  print (product . take 2 . reverse . sort $ totals)

-- print (map (map items . fst . last) game)