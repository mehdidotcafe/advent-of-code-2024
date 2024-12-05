import Data.List (lines, map, intersect)
import Data.List.Split (splitOn)
import Data.Map.Strict (fromListWith, (!?))

splitAndRead c = map (read :: String -> Int) . splitOn c

listRulesToMap = fromListWith (++) . map ((\ [x,y] -> (y, [x])) . splitAndRead "|")

extractRulesAndUpdates = (\ (r, u) -> (listRulesToMap r, map (splitAndRead ",") $ tail u)) . break null . lines

getMiddleElem = map (\ l -> l !! (length l `div` 2))

fixUpdates (r, u) = let
  filterUpdates' [] = []
  filterUpdates' (x:xs) = case r !? x of
    Nothing -> x : filterUpdates' xs
    Just rl -> if null (rl `intersect` xs) then x : filterUpdates' xs else filterUpdates' (xs ++ [x])
  in map snd $ filter (uncurry (/=)) $ map (\ line -> (line, filterUpdates' line)) u

main =
  getContents
    >>=  print . sum . getMiddleElem . fixUpdates . extractRulesAndUpdates