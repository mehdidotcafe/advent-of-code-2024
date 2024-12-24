import qualified Data.List.Split as LS (splitOn)
import qualified Data.List as L (permutations, intercalate, sort, sortOn, elem)
import qualified Data.Map as M (fromListWith, (!), keys)
import qualified Data.Algorithm.MaximalCliques as MC (getMaximalCliques)

parse input = let
  rows = concatMap (L.permutations . LS.splitOn "-") $ lines input
  in M.fromListWith (++) $ map (\ [k, v] -> (k, [v])) rows

solve graph = L.intercalate "," $ L.sort $ last $ L.sortOn length cliques
  where cliques = MC.getMaximalCliques (\ a b -> L.elem b $ graph M.! a) $ M.keys graph

main =
  getContents
    >>=  print . solve  . parse 
 