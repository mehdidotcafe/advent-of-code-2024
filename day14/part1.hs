import qualified Data.List as L (partition)
import qualified Data.List.Split as LS (splitOn)

tuplify l = (head l, head $ tail l)

parse = let
  extract = (read :: String -> Int) . dropWhile (flip elem ['p', 'v', '='])
  in map (tuplify . map (tuplify . map (extract) . LS.splitOn ",") . LS.splitOn " ") . lines 

solve t (width, height) d = let
  sumQuadrants = product . map (length)
  makeQuadrants = concatMap (( \ (x, y) -> [x, y]) . L.partition ((<= (height `div` 2)) . snd)) . ( \ (x, y) -> [x, y]) . L.partition ((<= (width `div` 2)) . fst)
  remMiddleDrones (x, y) = (even width || x /= (width `div` 2)) && (even height || y /= (height `div` 2))
  calc a va b = (a + va * t) `mod` b
  computeDronePos ((dx, dy), (vx, vy)) = (calc dx vx width, calc dy vy height)
  in sumQuadrants $ makeQuadrants $ filter (remMiddleDrones) $ map (computeDronePos) d
main =
  getContents
    >>=  print . solve 100 (101, 103) . parse 
