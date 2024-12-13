import qualified Data.List  as L ((\\))
import qualified Data.Map.Strict as M (fromList, (!?), empty, insert, Map)
import qualified Data.Set as S (empty, notMember, member, insert, union, Set)

parse input = let
  rows = lines input
  height = length rows
  width = length $ head rows
  in ((height, width), M.fromList $ concatMap (\ (y, l) -> map (\ (x, c) -> ((x, y), c)) l) $ zip [0..] $ map (zip [0..]) rows)

vectors = [(0, (-1)), (1, 0), (0, 1), ((-1), 0)]

add (a, b) (c, d) = (a + c, b + d)
union (a, b, c) (d, e, f) = (a + d, b + e, c `S.union` f)
computePrice (a, b) (c, d, e) = (a + (c * d), b `S.union` e)

solve (height, width) board = let
  hasSamePlant trail p v = board M.!? (add p v) /= Nothing && board M.!? p == board M.!? (add p v)
  solve' (x, y) gTrail lTrail
    | (x, y) `S.member` gTrail = (0, 0, S.empty)
    | otherwise = let
      siblingPosWithSamePlant = filter (hasSamePlant gTrail (x, y)) vectors
      siblingPosNotVisited = filter (flip S.notMember gTrail . add (x, y)) siblingPosWithSamePlant
      siblingFences = (vectors L.\\ siblingPosWithSamePlant)
      freeFencesCount = sum $ map (\ f -> length $ filter (\ (vx, vy) -> (x + vx, y + vy, f) `S.member` lTrail) siblingPosWithSamePlant) siblingFences
      gTrail' = (S.insert (x, y) gTrail)
      lTrail' = foldr (\ fp acc -> S.insert (x, y, fp) acc) lTrail siblingFences
      in foldr (\ vec (p,a,t) -> union (p,a,t) (solve' (add (x, y) vec) (t `S.union` gTrail') lTrail')) (length siblingFences - freeFencesCount, 1, gTrail') siblingPosNotVisited
  in foldr (\ coord (c,t) -> computePrice (c,t) (solve' coord t S.empty)) (0, S.empty) (concatMap (zip [0..width - 1] . repeat) $ [0..height - 1])

main =
  getContents
    >>=  print . fst . uncurry solve . parse