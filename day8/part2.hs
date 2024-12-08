import qualified Data.Map.Strict as M (fromListWith, mapWithKey, map, foldr)
import qualified Data.Set as S (fromList, union, empty, singleton)

parse input = let 
  allLines = lines input
  height = length allLines
  width = length $ head allLines
  toCoordMap = M.fromListWith (++) . map (\ t -> (fst t, [snd t])) . filter ((/= '.') . fst) . concatMap (\ (y, l) -> zip l (zip [0..] (repeat y))) . zip [0..]
  in (height, width, toCoordMap allLines)

isInBound (height, width) (x,y) = x >= 0 && x < width && y >= 0 && y < height

calcAntinodesInBound hw (ax, ay) (bx, by) = (ax, ay) : (takeWhile (isInBound hw) $ map (\ i -> (bx + (bx - ax) * i, by - (ay - by) * i)) [1..])

solve (height, width, coordMap) = let
    hw = (height, width)
    calcAntinodes [] = S.empty
    calcAntinodes [x] = S.singleton x
    calcAntinodes (x:xs) = (S.fromList (concat (zipWith (\ a b -> (calcAntinodesInBound hw a b) ++ (calcAntinodesInBound hw b a)) (repeat x) xs))) `S.union` (calcAntinodes xs)
  in length $ M.foldr (\ acc s -> acc `S.union` s) S.empty $ M.map calcAntinodes coordMap

main =
  getContents
    >>=  print . solve . parse
