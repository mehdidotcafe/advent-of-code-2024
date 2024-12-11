import Data.Char (digitToInt)
import Data.List (findIndices)
import Data.Array.Unboxed (listArray, elems, UArray, (!), (!?))

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
    Nothing -> 0
    Just npDigit -> if (tmap ! pos) + 1 == npDigit then solve' nextPos else 0
    where nextPos = pos + vector
  solve' pos
    | tmap ! pos == 9 = 1
    | otherwise = sum $ map (calcNextPos pos) [-width, width, -1, 1]
  in sum $ map solve' th

main =
  getContents
    >>=  print . solve . parse
