import qualified Data.List.Split as LS (splitOn)
import qualified Data.Map as M (fromList, empty, unionWith, Map, (!))
import qualified Data.Set as S (empty, notMember, Set, insert, fromList, member)
import qualified Data.Heap as H (singleton, MinPrioHeap, view, union, fromList)
import qualified Data.Maybe as MA (catMaybes)

side = 71

parse l = let
  rows = lines l
  byteCoords = S.fromList $ take 1024 $ map ((\ [x, y] -> y * side + x) . map (read :: String -> Int) . LS.splitOn ",") rows
  topEdge p = if p - side >= 0 then Just $ p - side else Nothing
  rightEdge p = if (p + 1) `div` side == p `div` side then Just $ p + 1 else Nothing
  bottomEdge p = if p + side < side * side then Just $ p + side else Nothing
  leftEdge p = if (p - 1) `div` side == p `div` side then Just $ p - 1 else Nothing
  makeGraph = M.fromList [ (p, map (\ edge -> (edge, 1)) $ filter (flip S.notMember byteCoords) $ MA.catMaybes [topEdge p, rightEdge p, bottomEdge p, leftEdge p]) | p <- [0..side * side - 1], p `S.notMember` byteCoords]
  in makeGraph

findShortestDistance graph start end = let
  discoverNode :: H.MinPrioHeap Int Int -> S.Set Int -> M.Map Int Int -> M.Map Int Int 
  discoverNode queue visited dist = case H.view queue of
    Nothing -> dist
    Just ((cost, node), queue') -> if node == end
      then dist
      else if S.member node visited
      then discoverNode queue' visited dist
      else let
        unvisitedNodes = filter (flip S.notMember visited . fst) $ graph M.! node
        queue'' = queue' `H.union` H.fromList (map (\ (n, c) -> (c + cost, n)) unvisitedNodes)
        visited' = S.insert node visited
        dist' = M.unionWith min dist $ M.fromList (map (\ (n, c) -> (n, c + cost)) unvisitedNodes)
        in discoverNode queue'' visited' dist'
  in (discoverNode (H.singleton (0, start)) (S.empty) M.empty) M.! end

solve graph = findShortestDistance graph 0 (side * side - 1)

main =
  getContents
    >>=  print . solve . parse 
