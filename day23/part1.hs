import qualified Data.List.Split as LS (splitOn)
import qualified Data.List as L (permutations, foldr, elem, sort)
import qualified Data.Map.Strict as M (fromList, fromListWith, (!), foldr, foldrWithKey)
import qualified Data.Set as S (fromList, empty, union, singleton)

parse l = let
  rows = concatMap (L.permutations . LS.splitOn "-") $ lines l
  in M.fromListWith (++) $ map (\ [k, v] -> (k, [v])) rows

solve graph = let
  solve' node = let
    children = graph M.! node
    in L.foldr (\ child s -> let
      gChildren = graph M.! child
      in s `S.union` L.foldr (\ gChild s -> if node `L.elem` (graph M.! gChild)
        then s `S.union` (S.singleton $ L.sort $ [node, child, gChild]) 
        else s
      ) S.empty gChildren) S.empty children
  in length $ M.foldrWithKey (\ key _ connections -> if head key == 't' 
      then connections `S.union` (solve' key) 
      else connections) S.empty graph

main =
  getContents
    >>=  print . solve  . parse 
