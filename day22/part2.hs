import qualified Data.Bits as B (xor)
import qualified Data.Map as M (empty, unions, insert, unionsWith, elems, member)
import qualified Data.List as L (foldl, maximum, sort)

parse = map (read :: String -> Int) . lines

mix nb nnb = nb `B.xor` nnb

prune nb = nb `mod` 16777216

smul m nb = prune $ mix nb $ nb * m

sdiv m nb = prune $ mix nb $ nb `div` m

calc nb = smul 2048 $ sdiv 32 $ smul 64 nb

arePrevsInvalid (a, b, c, d) = a == -1 || b == -1 || c == -1 || d == -1

solve initialSecrets = let
  solve' prevs@(a, b, c, d) changes n secret
    | n == -1 = changes
    | otherwise = let
        secretLastDigit = secret `mod` 10
        changesKey = (b - a, c - b, d - c, secretLastDigit - d)
        changes' = if arePrevsInvalid prevs || changesKey `M.member` changes
          then changes
          else M.insert changesKey secretLastDigit changes
        in solve' (b, c, d, secretLastDigit) changes' (n - 1) $ calc secret
  in L.maximum $ M.elems $ M.unionsWith (+) $ map (solve' (-1, -1, -1, -1) M.empty 2000) initialSecrets

main =
  getContents
    >>=  print . solve . parse 
