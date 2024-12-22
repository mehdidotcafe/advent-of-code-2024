import qualified Data.Bits as B (xor)

parse = map (read :: String -> Int) . lines

mix nb nnb = nb `B.xor` nnb

prune nb = nb `mod` 16777216

smul m nb = prune $ mix nb $ nb * m

sdiv m nb = prune $ mix nb $ nb `div` m

calc nb = smul 2048 $ sdiv 32 $ smul 64 nb

solve initialSecret = let
  solve' n secret
    | n == 0 = secret
    | otherwise = solve' (n - 1) $  calc secret
  in sum $ map (solve' 2000) initialSecret

main =
  getContents
    >>=  print . solve . parse 
