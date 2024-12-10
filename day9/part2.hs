import Data.Char (digitToInt)
import Data.List (group, findIndex, (!!), findIndices)

parse list = let 
  buildFullList [] _ _ = []
  buildFullList (x:xs) i isEven = replicate x (if isEven == 1 then i else -1) ++ buildFullList xs (i + isEven) ((isEven + 1) `mod` 2)
  groupedList = zip (group $ buildFullList (map digitToInt list) 0 1) [0..]
  in (groupedList, reverse groupedList)

replaceAt idx  y r xs = lft ++ y ++ r ++ rgt
  where (lft, (_:rgt)) = splitAt idx xs

replicateFreeSpace v = (replicate v (-1), (-1))

solve xs [] = xs
solve xs (y:ys)
  | head (fst y) == -1 = solve xs ys
  | otherwise =  case findIndex (\x -> head (fst x) == -1 && length (fst x) >= ly) xs of
  Nothing -> solve xs ys
  Just i ->  let 
      nxs = replaceAt i [y] [replicateFreeSpace (abs (ly - length (fst (xs !! i))) )] xs  
      in case findIndices (\x -> snd x == snd y) nxs of
        [] -> solve xs ys
        idc -> if i >= lidc then solve xs ys else solve (filter (not . null . fst) $ replaceAt lidc [replicateFreeSpace ly] [] nxs) ys
          where lidc = last idc
  where ly = length (fst y)

sumFullList l = sum $ map (\ (d, i) -> (max d 0) * i) $ zip l [0..]

main =
  getContents
    >>=  print . sumFullList . concatMap (fst) . (uncurry solve) . parse