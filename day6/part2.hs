import Data.Array.Unboxed (listArray, (!?), UArray)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Set (empty, insert, member, Set)

parse list = let
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

nextDirection d = (d + 1) `mod` 4

hasVisited trail pos direction = (pos, direction) `member` trail

inBound array pos nextPos height width
  | pos `div` width /= nextPos `div` width && pos `mod` height /= nextPos `mod` height = Nothing
  | otherwise = array !? nextPos

solve vectors  (pos, height, width, array) = let
  checkNextPosition (v:vs) trail direction (pos, height, width, array) obstaclePos
    | hasVisited trail nextPos direction = True
    | nextPos == obstaclePos = solve' (vs ++ [v]) ((pos,direction) `insert` trail) (nextDirection direction) (pos, height, width, array) obstaclePos
    | otherwise = solve' (v:vs) ((pos,direction) `insert` trail) direction (nextPos, height, width, array) obstaclePos
    where nextPos = pos + (v width)
  solve' :: [Int -> Int] -> Set (Int, Int) -> Int -> (Int, Int, Int, UArray Int Char) -> Int -> Bool
  solve' [] _ _ _ _ = False
  solve' (v:vs) trail direction (pos, height, width, array) obstaclePos = case inBound array pos (pos + (v width)) height width of
    Nothing -> False
    Just '#' -> solve' (vs ++ [v]) trail (nextDirection direction) (pos, height, width, array) obstaclePos
    _ -> checkNextPosition (v:vs) trail direction (pos, height, width, array) obstaclePos
  in length $ filter id $ map (solve' vectors empty 0 (pos, height, width, array)) [0..height * width - 1]
main =
  getContents
    >>=  print . solve [up, right, down, left] . parse
