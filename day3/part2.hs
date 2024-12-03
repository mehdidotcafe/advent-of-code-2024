import Text.Regex.TDFA ((=~))

data Context = Do | Dont deriving (Eq)

matchPattern :: String -> [[String]]
matchPattern pattern = pattern =~ "mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don\'t\\(\\)" :: [[String]]

reduceWithMultiply = foldr ((\ (x:y:xs) acc -> acc + x * y) . (map (read :: String -> Int) . tail)) 0

filterWithDont :: [[[Char]]] -> [[[Char]]]
filterWithDont p = let
  filterWithDont' [] c = []
  filterWithDont' (x:xs) c
    | head x == "do()" = filterWithDont' xs Do
    | c == Dont || head x == "don't()" = filterWithDont' xs Dont
    | otherwise = x : filterWithDont' xs Do
  in filterWithDont' p Do

main =
  getContents
    >>=  print . reduceWithMultiply . filterWithDont . matchPattern
