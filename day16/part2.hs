import qualified Data.List as L ((!!), partition, findIndex, foldl, minimumBy, filter, map)
import qualified Data.Map as M (fromList, (!), (!?), Map, unionWith, empty, member, notMember, insert)
import qualified Data.Set as S (empty, insert, Set, fromList, notMember, union, size, unions)
import qualified Data.Maybe as MA (catMaybes)
import qualified Data.Heap as H (singleton, MinPrioHeap, view, union, fromList)
import qualified Data.Maybe as MB (fromMaybe)
import qualified Data.Function as F (on)

data Direction = North | East | South | West deriving (Eq, Ord, Show)

genGraph (height, width) rows = let
  concatRows = concat rows
  (walls, cells) = L.partition ((== '#') . snd) $ zip [0..] $ concatRows
  cellsList = map fst cells
  initialPos = (MB.fromMaybe 0 $ L.findIndex (== 'S') concatRows, MB.fromMaybe 0 $ L.findIndex (== 'E') concatRows)
  wallsSet = S.fromList $ map fst walls
  topEdge p = if p - width >= 0 then Just $ (p - width, North) else Nothing
  rightEdge p = if (p + 1) `div` width == p `div` width then Just $ (p + 1, East) else Nothing
  bottomEdge p = if p + width < height * width then Just $ (p + width, South) else Nothing
  leftEdge p = if (p - 1) `div` width == p `div` width then Just $ (p - 1, West) else Nothing
  makeGraph = M.fromList [ (c, filter (flip S.notMember wallsSet . fst) $ MA.catMaybes [topEdge c, rightEdge c, bottomEdge c, leftEdge c]) | c <- cellsList, c `S.notMember` wallsSet]
  in (initialPos, makeGraph)

parse input = let
  rows = lines input
  height = length rows
  width = length $ head rows
  in genGraph (height, width) rows


findShortestDistances start end graph = let
  discoverNode :: H.MinPrioHeap Int (Int, S.Set Int, Int, Direction) -> M.Map (Int, Direction) Int -> M.Map Int Int -> [(S.Set Int, Int)] -> [(S.Set Int, Int)]
  discoverNode queue visited dist paths = case H.view queue of
    Nothing -> paths
    Just ((cost, (node, trail, score, direction)), queue') -> if node == end
      then discoverNode queue' visited dist ((trail,score):paths)
      else if M.member (node, direction) visited && visited M.! (node, direction) < score
      then discoverNode queue' visited dist paths
      else let
        computeExtraCost d = if d /= direction then 1000 else 0
        unvisited = L.filter (flip S.notMember trail . fst) $ graph M.! node
        queue'' = queue' `H.union` H.fromList (map (\ (n, d) -> (cost + 1 + computeExtraCost d, (n, S.insert n trail, cost + 1 + computeExtraCost d, d))) unvisited)
        visited' = M.insert (node, direction) cost visited
        dist' = M.unionWith min dist $ M.fromList (map (\ (n, c) -> (n, cost + 1)) unvisited)
        in discoverNode queue'' visited' dist' paths
  in discoverNode (H.singleton (0, (start, S.empty, 0, East))) M.empty M.empty []

solve (start, end) graph = let
  shortestDistances = findShortestDistances start end graph
  minimumScore = snd $ L.minimumBy (compare `F.on` snd) shortestDistances
  in 1 + (S.size $ S.unions $ L.map fst $ L.filter ((== minimumScore) . snd) shortestDistances)

main =
  getContents
    >>=  print . (uncurry solve) . parse 
