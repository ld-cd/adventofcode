import Data.Char (isUpper, ord)
import Data.Functor ((<$>))
import Data.Text (pack, singleton, splitOn, unpack)
import System.Environment (getArgs)
import System.IO (readFile)

parseRange r = (read . unpack . head $ es, read . unpack . last $ es) where es = splitOn (singleton '-') r

parseLine "" = ((0, 1), (2, 3))
parseLine l = (parseRange . head $ rs, parseRange . last $ rs) where rs = splitOn (singleton ',') (pack l)

contains :: ((Int, Int), (Int, Int)) -> Int
contains r
  | al <= bl && ah >= bh = 1
  | bl <= al && bh >= ah = 1
  | otherwise = 0
  where
    al = fst . fst $ r
    ah = snd . fst $ r
    bl = fst . snd $ r
    bh = snd . snd $ r

overlap :: ((Int, Int), (Int, Int)) -> Int
overlap r
  | al <= bl && ah >= bl = 1
  | bl <= al && bh >= al = 1
  | otherwise = 0
  where
    al = fst . fst $ r
    ah = snd . fst $ r
    bl = fst . snd $ r
    bh = snd . snd $ r

main = do
  (name : _) <- getArgs
  results <- lines <$> readFile name
  print . sum $ map (contains . parseLine) results
  print . sum $ map (overlap . parseLine) results