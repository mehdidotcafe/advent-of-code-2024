import qualified Data.Map.Strict as M (fromListWith, mapWithKey, map, foldr)
import qualified Data.Set as S (fromList, union, empty)

parse input = let 
  allLines = lines input
  height = length allLines
  width = length $ head allLines
  toCoordMap = M.fromListWith (++) . map (\ t -> (fst t, [snd t])) . filter ((/= '.') . fst) . concatMap (\ (y, l) -> zip l (zip [0..] (repeat y))) . zip [0..]
  in (height, width, toCoordMap allLines)

isInBound (height, width) (x,y) = x >= 0 && x < width && y >= 0 && y < height

calcBotAntinodes (ax, ay) (bx, by) = (ax - (bx - ax), ay - (by - ay))
calcTopAntinodes (ax, ay) (bx, by) = (ax + (bx - ax) * 2, ay + (by - ay) * 2)

solve (height, width, coordMap) = let
    calcAntinodes [] = S.empty
    calcAntinodes (x:xs) = (S.fromList (filter (isInBound (height, width)) $ concat (zipWith (\ a b -> [calcBotAntinodes a b, calcTopAntinodes a b]) (repeat x) xs))) `S.union` (calcAntinodes xs)
  in length $ M.foldr (\ acc s -> acc `S.union` s) S.empty $ M.map calcAntinodes coordMap

main =
  getContents
    >>=  print . solve . parse
