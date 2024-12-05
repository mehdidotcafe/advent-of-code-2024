import Data.List (lines, map, intersect)
import Data.List.Split (splitOn)
import Data.Map.Strict (fromListWith, (!?))
import Debug.Trace

splitAndRead c = map (read :: String -> Int) . splitOn c

listRulesToMap = fromListWith (++) . map ((\ [x,y] -> (y, [x])) . splitAndRead "|")

extractRulesAndUpdates = (\ (r, u) -> (listRulesToMap r, map (splitAndRead ",") $ tail u)) . break null . lines

getMiddleElem = map (\ l -> l !! (length l `div` 2))

filterUpdates (r, u) = let
  filterUpdates' [] = True
  filterUpdates' (x:xs) = case r !? x of
    Nothing -> filterUpdates' xs
    Just rr -> null (rr `intersect` xs) && filterUpdates' xs
  in filter filterUpdates' u

main =
  getContents
    >>=  print . sum . getMiddleElem . filterUpdates . extractRulesAndUpdates
