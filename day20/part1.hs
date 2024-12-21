import qualified Data.List as L ((!!), partition, findIndex, intersect)
import qualified Data.Map as M (fromList, (!), (!?), Map, unionWith, empty, union, notMember, member)
import qualified Data.Set as S (member, empty, insert, Set, fromList, notMember, union)
import qualified Data.Maybe as MB (catMaybes, fromMaybe)
import qualified Data.Heap as H (singleton, MinPrioHeap, view, union, fromList)

addX (height, width) x p = if (p + x) `div` width == p `div` width then Just $ p + x else Nothing
addY (height, width) x p = if p + x >= 0 && p + x <= height * width then Just $ p + x else Nothing

genGraph hw@(height, width) rows = let
  concatRows = concat rows
  (walls, cells) = L.partition ((== '#') . snd) $ zip [0..] $ concatRows
  cellsList = map fst cells
  wallsSet = S.fromList $ map fst walls
  graph = M.fromList [ (c, map (\ e -> (e, 1)) $ filter (flip S.notMember wallsSet) $ MB.catMaybes [addY hw (-width) c, addX hw 1 c, addY hw width c, addX hw (-1) c]) | c <- cellsList, c `S.notMember` wallsSet]
  initialPos = (MB.fromMaybe 0 $ L.findIndex (== 'S') concatRows, MB.fromMaybe 0 $ L.findIndex (== 'E') concatRows)
  in (hw, initialPos, graph)

parse input = let
  rows = lines input
  height = length rows
  width = length $ head rows
  in genGraph (height, width) rows

findShortestDistance start end graph = let
  discoverNode :: H.MinPrioHeap Int Int -> S.Set Int -> M.Map Int Int -> [Int] -> (M.Map Int Int,  [Int])
  discoverNode queue visited dist prev = case H.view queue of
    Nothing -> (dist, prev)
    Just ((cost, node), queue') -> if node == end
      then (dist, prev)
      else if S.member node visited
      then discoverNode queue' visited dist prev
      else let
        unvisited = filter (flip S.notMember visited . fst) $ graph M.! node
        queue'' = queue' `H.union` H.fromList (map (\ (n, c) -> (c + cost, n)) unvisited)
        visited' = S.insert node visited
        dist' = M.unionWith min dist $ M.fromList (map (\ (n, c) -> (n, c + cost)) unvisited)
        prev'= prev ++ map (fst) unvisited 
        in discoverNode queue'' visited' dist' prev'
  (dist, prev) = discoverNode (H.singleton (0, start)) S.empty M.empty []
  in (dist M.!? end, start : prev)

solve (hw@(height, width), (start, end), graph) = let
  (cost, path) = findShortestDistance start end graph
  pathCost :: M.Map Int Int
  pathCost = M.fromList $ flip zip [0..] $ reverse path
  solve' pos = let
    getCheatPosition add v = if  (pos + v * 3) `M.member` graph && (pos + v * 2) `M.notMember`graph  &&  (pos + v) `M.notMember` graph 
      then case add hw (v * 3) pos of 
        Nothing -> Nothing
        Just v -> Just $ (v, 3)
      else if (pos + v * 2) `M.member` graph && (pos + v) `M.notMember` graph
        then case add hw (v * 2) pos of 
          Nothing -> Nothing
          Just v -> Just $ (v, 2) 
        else Nothing
    wallNeighborsX = MB.catMaybes $ map (getCheatPosition addX) [-1, 1]
    wallNeighborsY = MB.catMaybes $ map (getCheatPosition addY) [-width, width]
    cheatNeighbors = wallNeighborsX ++ wallNeighborsY
    in S.fromList $ filter ((>= 100) . snd) $ map (\ (npos, cost) -> ((pos, npos), (pathCost M.! pos) - (pathCost M.! npos) - cost)) cheatNeighbors
  in length $ foldl (\ acc pos -> acc `S.union` solve' pos) S.empty path
main =
  getContents
    >>=  print . solve . parse 
