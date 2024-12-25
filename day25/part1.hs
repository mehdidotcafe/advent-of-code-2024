import qualified Data.List as L (transpose, partition, length, filter, map, all, concatMap)
import qualified Data.List.Split as LS (splitOn)

parse input = let
  computeHeight pin = map (L.map (flip (-) 1 . L.length . L.filter (== '#'))) pin
  cols = map L.transpose $ LS.splitOn [""] $ lines input
  (locks, keys) = L.partition ((== '#') . head . head) cols
  in (computeHeight locks, computeHeight keys)

solve (locks, keys) = L.length $ L.concatMap (\ key -> L.filter (L.all ((<= 5) . uncurry (+)) . zip key) locks) keys 

main =
  getContents
    >>=  print . solve  . parse 
