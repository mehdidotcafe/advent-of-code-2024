import qualified Data.List.Split as LS (splitWhen)
import qualified Data.Char as C (isDigit)

parse l = let
  extract = (read :: String -> Int) . dropWhile (not . C.isDigit)
  extractXY r = let 
    (x, y) = break (== ',') r
    in (extract x, extract y)
  in map (\ [a, b, p] -> (extractXY a, extractXY b, extractXY p)) $ LS.splitWhen (== "") $ lines l

solve d = let
  solve' ((xa, ya), (xb, yb), (xp, yp)) = let
    d = xa * yb - xb * ya 
    m = (yb * xp - xb * yp)
    n = (xa * yp - ya * xp)
    in if d == 0 || m `mod` d /= 0 || n `mod` d /= 0 then 0 else 3 * (m `div` d) + (n `div` d)
  in sum $ map (solve') d

main =
  getContents
    >>=  print . solve . parse