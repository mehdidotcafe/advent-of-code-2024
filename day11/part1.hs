import Data.List (splitAt)

parse = map (read :: String -> Int) . words

defaultOnEmpty xs = if null xs then ['0'] else xs

split xAsStr = let 
  xLength = length xAsStr
  (left, right) = splitAt (xLength `div` 2)  xAsStr
  in map ((read :: String -> Int) . defaultOnEmpty . dropWhile (==  '0')) [left, right]

change x
  | x == 0 = [1]
  | even (length xAsStr) = split xAsStr
  | otherwise = [x * 2024]
  where xAsStr = show x

solve _ [] = []
solve blink (x:xs)
  | blink == 0 = (x:xs)
  | otherwise = solve (blink - 1) (change x) ++ solve blink xs

main =
  getContents
    >>=  print . length . solve 25 . parse