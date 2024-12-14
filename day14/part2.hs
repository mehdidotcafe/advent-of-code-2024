import qualified Data.List as L (partition, intersperse)
import qualified Data.List.Split as LS (splitOn)
import qualified Data.Set as S (fromList, member)

tuplify l = (head l, head $ tail l)

parse = let
  extract = (read :: String -> Int) . dropWhile (flip elem ['p', 'v', '='])
  in map (tuplify . map (tuplify . map (extract) . LS.splitOn ",") . LS.splitOn " ") . lines 

solve t (width, height) d = let
  makeMap i coords = show i : map (map (\ c -> if c `S.member` sCoords then '*' else '.')) (map (zip [0..width - 1] . repeat) $ [0..height - 1])
    where sCoords = S.fromList coords
  computeDronePos i ((dx, dy), (vx, vy)) = let
    calc a va b = (a + va * i) `mod` b
    in (calc dx vx width, calc dy vy height)
  solve' i = makeMap i $ map (computeDronePos i) d
  in map solve' [0..t]
main =
  getContents
    >>=  mapM (mapM print) . solve 10000 (101, 103) . parse 
