import Data.Char (digitToInt)

parse list = let 
  buildFullList [] _ _ = []
  buildFullList (x:xs) i isEven = replicate x (if isEven == 1 then i else -1) ++ buildFullList xs (i + isEven) ((isEven + 1) `mod` 2)
  fullList = buildFullList (map digitToInt list) 0 1
  in (fullList, reverse fullList)

solve [] _ = []
solve _ [] = []
solve [x] _ = [x]
solve (x:xs) (y:ys)
  | x /= -1 = x : solve (xs) (y:ys)
  | y == -1 = solve (x:(init xs)) (ys)
  | otherwise = y : (solve (init xs) (ys))

sumFullList l = sum $ map (\ (d, i) -> (max d 0) * i) $ zip l [0..]

main =
  getContents
    >>=  print . sumFullList . (uncurry solve) . parse