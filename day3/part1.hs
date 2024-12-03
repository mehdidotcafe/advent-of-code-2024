import Text.Regex.TDFA ((=~))

matchPattern :: String -> [[String]]
matchPattern pattern = pattern =~ "(mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\))" :: [[String]]

main =
  getContents
    >>=  print . foldr ((\ (x:y:xs) acc -> acc + x * y) . (map (read :: String -> Int) . tail)) 0 . matchPattern
