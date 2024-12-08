import Data.Array.Unboxed (listArray, (!?), UArray)
import Data.List (elemIndex, nub)
import Data.Maybe (fromMaybe)

listToArray list = let
  l = lines list
  height = length l
  width = length $ head l
  cl = concat l
  initialPosition = fromMaybe 0 (elemIndex '^' cl)
  in (initialPosition, height, width, listArray (0, width * height - 1) (concat l))

up width = -width
right width = 1
down width = width
left width = -1

inBound array pos nextPos height width
  | pos `div` width /= nextPos `div` width && pos `mod` height /= nextPos `mod` height = Nothing
  | otherwise = array !? nextPos

solve :: [Int -> Int] -> (Int, Int, Int, UArray Int Char) -> [Int]
solve [] _ = []
solve (v:vs) (pos, height, width, array) = case inBound array pos (pos + (v width)) height width of
  Nothing -> [pos]
  Just '#' -> solve (vs ++ [v]) (pos, height, width, array)
  _ -> pos : solve (v:vs) (pos + (v width), height, width, array)

main =
  getContents
    >>=  print . length . nub . solve [up, right, down, left] . listToArray
