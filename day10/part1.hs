import Data.Char (digitToInt)
import Data.List (findIndices)
import Data.Array.Unboxed (listArray, elems, UArray, (!), (!?))
import Data.Set (empty, singleton, union)

parse list = let
  listAsLines = lines list
  height = length listAsLines
  width = length $ head listAsLines
  tmap = listArray(0, height * width - 1) $ map digitToInt (concat listAsLines)
  th = findIndices (== 0) $ elems tmap
  in (width, height, th, tmap)

inBounds array pos nextPos height width
  | pos `div` width /= nextPos `div` width && pos `mod` height /= nextPos `mod` height = Nothing
  | otherwise = array !? nextPos

solve :: (Int, Int, [Int], UArray Int Int) -> Int
solve (width, height, th, tmap) = let
  calcNextPos pos vector = case inBounds tmap pos nextPos height width of
    Nothing -> empty
    Just npDigit -> if (tmap ! pos) + 1 == npDigit then solve' nextPos else empty
    where nextPos = pos + vector
  solve' pos
    | tmap ! pos == 9 = singleton pos
    | otherwise = foldr ( union ) empty $ map (calcNextPos pos) [-width, width, -1, 1]
  in sum $ map (length . solve') th

main =
  getContents
    >>=  print . solve . parse
