import qualified Data.List as L (splitAt)
import qualified Data.Map.Strict as M (fromListWith, foldrWithKey, singleton, empty, unionWith, elems, Map)

parse = M.fromListWith (+) . flip zip (repeat 1) . map (read :: String -> Int) . words

defaultOnEmpty xs = if null xs then ['0'] else xs

split o xAsStr = let 
  xLength = length xAsStr
  (left, right) = L.splitAt (xLength `div` 2)  xAsStr
  in M.fromListWith (+) $ map ((\x -> (x, o)) . (read :: String -> Int) . defaultOnEmpty . dropWhile (==  '0')) [left, right]

change o x
  | x == 0 = M.singleton 1 o
  | even (length xAsStr) = split o xAsStr
  | otherwise = M.singleton (x * 2024) o
  where xAsStr = show x

solve blink m
  | blink == 0 = m
  | otherwise = solve (blink - 1) um
  where um = M.foldrWithKey (\ x o acc -> M.unionWith (+) acc (change o x)) M.empty m

format = sum . M.elems

main =
  getContents
    >>=  print . format . solve 75 . parse