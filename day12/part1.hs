import qualified Data.List  as L (foldr)
import qualified Data.Map.Strict as M (fromList, (!?), Map)
import qualified Data.Set as S (empty, notMember, member, insert, union, Set)

parse input = let
  rows = lines input
  height = length rows
  width = length $ head rows
  in ((height, width),  M.fromList $ concatMap (\ (y, l) -> map (\ (x, c) -> ((x, y), c)) l) $ zip [0..] $ map (zip [0..]) rows)

vectors = [(0, (-1)), (1, 0), (0, 1), ((-1), 0)]

solve (height, width) board = let
  add (a, b) (c, d) = (a + c, b + d)
  union (a, b, c) (d, e, f) = (a + d, b + e, c `S.union` f)
  calcPrice (a, b) (c, d, e) = (a + (c * d), b `S.union` e)
  hasSamePlant trail p v = board M.!? (add p v) /= Nothing && board M.!? p == board M.!? (add p v)
  solve' (x, y) trail
    | (x, y) `S.member` trail = (0, 0, S.empty)
    | otherwise = let
      siblingPosWithSamePlant = filter (hasSamePlant trail (x, y)) vectors
      siblingPosNotVisited = filter ((\ v -> (add (x, y) v) `S.notMember` trail)) siblingPosWithSamePlant
      trail' = (S.insert (x, y) trail)
      in L.foldr (\ vec (p,a,t) -> union (p,a,t) (solve' (add (x, y) vec) (t `S.union` trail'))) (4 - length siblingPosWithSamePlant, 1, trail') siblingPosNotVisited
  in L.foldr (\ coord (c,t) -> calcPrice (c,t) (solve' coord t)) (0, S.empty) (concatMap (zip [0..width - 1] . repeat) $ [0..height - 1])

main =
  getContents
    >>=  print . fst . uncurry solve . parse