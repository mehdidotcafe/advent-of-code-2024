import Data.List (map, sort, foldr)
import Data.Tuple (fst, snd)

tuplify [] = error "Empty list"
tuplify [_] = error "Need two elements in array to construct tuple"
tuplify (x:y:_) = (x, y)

calc p = let
  l = sort $ map fst p
  r = sort $ map snd p
  in foldr (\ (a, b) acc -> acc + abs (a - b)) 0 $ zip l r

main =
  getContents
    >>=  print . calc . map (tuplify . (map (read :: String -> Int) . words)) . lines
