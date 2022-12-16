import Data.Functor ((<$>))
import Data.Map qualified as Map
import System.Environment (getArgs)
import System.IO (readFile)
import Text.Parsec (parse)

data CD
  = Rel String
  | Absolute
  | Up
  deriving (Show)

parseCD "/" = ChangeDir Absolute
parseCD ".." = ChangeDir Up
parseCD n = ChangeDir . Rel $ n

data Line
  = ChangeDir CD
  | List
  | FileL File
  | DirL String
  deriving (Show)

data File = File {size :: Int, name :: String} deriving (Show)

parseCMD cs
  | head cs == "ls" = List
  | otherwise = parseCD . last $ cs

parseLine l
  | head ws == "$" = parseCMD . tail $ ws
  | head ws == "dir" = DirL . last $ ws
  | otherwise = FileL File {size = read . head $ ws, name = last ws}
  where
    ws = words l

updateDir current Absolute = []
updateDir current Up = drop 1 current
updateDir current (Rel n) = n : current

data Dir = Dir {path :: [String], files :: [File], subdirs :: [String]} deriving (Show)

getDir p [] = (Dir {path = p, files = [], subdirs = []}, [])
getDir p (DirL d : ls) =
  (Dir {path = p, files = fs, subdirs = d : ds}, rls)
  where
    (Dir {path = _, files = fs, subdirs = ds}, rls) = getDir p ls
getDir p (FileL f : ls) =
  (Dir {path = p, files = f : fs, subdirs = ds}, rls)
  where
    (Dir {path = _, files = fs, subdirs = ds}, rls) = getDir p ls
getDir p ls = (fst (getDir p []), ls)

getDirs cd (List : ls) = dir : getDirs cd rls
  where
    (dir, rls) = getDir cd ls
getDirs cd (ChangeDir c : ls) = getDirs (updateDir cd c) ls
getDirs cd [] = []

dirSize :: Map.Map [String] Dir -> [String] -> Int
dirSize dm p = sum (map size (files d)) + sum (map (dirSize dm . (: p)) (subdirs d))
  where
    d = dm Map.! p

main = do
  (name : _) <- getArgs
  results <- lines <$> readFile name
  let parsedlines = map parseLine results
  let dirs = getDirs [] parsedlines
  let dirmap = Map.fromList [(path i, i) | i <- dirs]
  let sizes = map (dirSize dirmap) (Map.keys dirmap)
  let needed = 30000000 - (70000000 - maximum sizes)
  print . sum $ filter (<= 100000) sizes
  print needed
  print . minimum $ filter (>= needed) sizes