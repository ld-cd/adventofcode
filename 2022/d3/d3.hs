import Data.Char (isUpper, ord)
import Data.Functor ((<$>))
import Data.Set (fromList, intersection, toList)
import System.Environment (getArgs)
import System.IO (readFile)

getScore c = ord c - if isUpper c then 64 - 26 else 96

trip [] = []
trip l = [fromList (map getScore i) | i <- take 3 l] : trip (drop 3 l)

main = do
  (name : _) <- getArgs
  results <- lines <$> readFile name
  print
    . sum
    $ [ sum
          . toList
          $ ( fromList (map getScore (take (length l `div` 2) l))
                `intersection` fromList (map getScore (drop (length l `div` 2) l))
            )
        | l <- results
      ]
  print . sum $ [sum . toList $ foldl1 intersection g | g <- trip results]