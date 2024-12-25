import qualified Data.List as L ((!!), partition, findIndex)
import qualified Data.Map as M (fromList, (!), (!?), Map, unionWith, empty)
import qualified Data.Set as S (member, empty, insert, Set, fromList, notMember)
import qualified Data.Maybe as MA (catMaybes)
import qualified Data.Heap as H (singleton, MinPrioHeap, view, union, fromList)
import qualified Data.Maybe as MB (fromMaybe)

data Direction = North | East | South | West deriving (Eq)

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


findShortestDistance start end graph = let
  discoverNode :: H.MinPrioHeap Int (Int, Direction) -> S.Set Int -> M.Map Int Int -> M.Map Int Int
  discoverNode queue visited dist = case H.view queue of
    Nothing -> dist
    Just ((cost, (node, direction)), queue') -> if node == end
      then dist
      else if S.member node visited
      then discoverNode queue' visited dist
      else let
        computeExtraCost d = if d /= direction then 1000 else 0
        unvisited = filter (flip S.notMember visited . fst) $ graph M.! node
        queue'' = queue' `H.union` H.fromList (map (\ (n, d) -> (cost + 1 + computeExtraCost d, (n, d))) unvisited)
        visited' = S.insert node visited
        dist' = M.unionWith min dist $ M.fromList (map (\ (n, c) -> (n, cost + 1)) unvisited)
        in discoverNode queue'' visited' dist'
  in (discoverNode (H.singleton (0, (start, East))) S.empty M.empty) M.!? end


solve (start, end) graph = findShortestDistance start end graph

main =
  getContents
    >>=  print . (uncurry solve)  . parse 
