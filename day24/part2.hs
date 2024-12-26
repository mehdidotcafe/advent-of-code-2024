import qualified Data.List.Split as LS (splitOn)
import qualified Data.List as L (isPrefixOf, filter, sort, foldl, sortOn, (!!), foldr, intercalate)
import qualified Data.Map.Strict as M (Map, (!), fromList, filter, foldl, unionsWith, elems, singleton, foldr)
import qualified Data.Bits as B (xor, shiftL, (.|.))

data LazyNodeGate = XOR | AND | OR deriving (Eq, Show)
data Node = LazyNode { left :: String, right :: String, gate :: LazyNodeGate, lName :: String } | EagerNode { output :: Bool, eName :: String } deriving (Eq, Show)

isLazyNode :: Node -> Bool
isLazyNode (LazyNode _ _ _ _) = True
isLazyNode _ = False

compute :: Node -> M.Map String Node -> (Bool, M.Map String Node)
compute (LazyNode left right gate n) graph = let
    gateOutput = case gate of
      XOR -> leftNodeGate `B.xor` rightNodeGate
      AND -> leftNodeGate && rightNodeGate
      OR -> leftNodeGate || rightNodeGate
    (leftNodeGate, leftGraph) = compute (graph M.! left) graph
    (rightNodeGate, rightGraph) = compute (graph M.! right) graph
    unionGraphs = M.unionsWith (\ a b -> if isLazyNode a then b else a) [graph, leftGraph, rightGraph, M.singleton left $ EagerNode leftNodeGate left, M.singleton right $ EagerNode rightNodeGate right]
  in  (gateOutput, unionGraphs)
compute (EagerNode output n) graph = (output, graph)

name :: Node -> String
name (LazyNode _ _ _ lName) = lName 
name (EagerNode _ eName) = eName

parse input = let
  gatesAssoc = M.fromList [("AND", AND), ("XOR", XOR), ("OR", OR)] 
  rows = lines input
  [eagerRows, lazyRows] = LS.splitOn [""] rows
  lazyNodes = map ((\ [left, gate, right, _, name] -> LazyNode left right (gatesAssoc M.! gate) name) . LS.splitOn " ") lazyRows
  eagerNodes = map ((\ [name, output] -> EagerNode (output == "1") name) . LS.splitOn ": ") eagerRows
  in M.fromList $ map (\ n -> (name n, n)) (lazyNodes ++ eagerNodes) 

recShow i graph node@(EagerNode _ eName) = replicate i '\t' ++ name node ++ " " ++ "EAGER"
recShow i graph node@(LazyNode left right gate lName)
  | i <= 2 = replicate i '\t' ++ (name node ++ " " ++ show gate) ++ "\n" ++ (recShow (i + 1) graph (graph M.! left)) ++ "\n" ++ (recShow (i + 1) graph (graph M.! right) )
  | otherwise = replicate i '\t' ++ (name node ++ " " ++ show gate)

solve graph = let
  boolToInt b = if b then 1 else 0
  updateGraphForNode :: (Int, M.Map String Node) -> Node -> (Int, M.Map String Node)
  updateGraphForNode (number, graph') node = let 
    (output, graph'') = compute node graph'
    in ((number `B.shiftL` 1) B..|. (boolToInt output), graph'')
  zNodes = L.sortOn name $ L.filter (\ node -> "z" `L.isPrefixOf` name node) $ M.elems graph
  graphForZNode = recShow 0 graph $ zNodes L.!! 45
  manuallyFoundPairs = L.intercalate "," $ L.sort $ concatMap (\ (a, b) -> [a, b]) [("z07", "shj"), ("tpk", "wkb"), ("z23", "pfn"), ("z27", "kcd")]
  in manuallyFoundPairs

main =
  getContents
    >>=  putStr  . solve  . parse 
