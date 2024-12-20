import qualified Data.List.Split as LS (splitOn)
import qualified Data.List as L (isPrefixOf, foldr)
import qualified Data.Map.Strict as M (empty, unions, singleton, Map, (!?))

parse l = let
  [towels, designs] = LS.splitOn [""] $ lines l
  in (map (dropWhile (== ' ')) $ LS.splitOn "," $ head towels, designs)

solve towels designs = let
  add r (a, b) = (a, r + b)
  solve' cache design = case cache M.!? design of
    Just v -> (cache, v)
    Nothing -> if null design
      then (cache, 1)
      else let
        validTowels = filter (flip L.isPrefixOf design) towels
        cachedSolve' towel (c, r) = let
          design' = flip drop design $ length towel
          (recurCache, recurR) = solve' c design'
          in (M.unions [c, M.singleton design' recurR,  recurCache], recurR + r)
      in L.foldr cachedSolve' (cache, 0) validTowels
  in snd $ L.foldr ( \ d (cache, r) -> r `add` solve' cache d) (M.empty, 0) designs

main =
  getContents
    >>=  print . (uncurry solve) . parse 
