import Data.Array (listArray, (!))

listToArray list = let
  l = lines list
  height = length l
  width = length $ head l
  in (height, width, listArray (0, width * height - 1) (concat l))

top height width grid idx = grid!idx == 'X' && idx - width * 3 >= 0  && grid!(idx - width) == 'M' && grid!(idx - width * 2) == 'A' &&  grid!(idx - width * 3) == 'S'
right height width grid idx = grid!idx == 'X' && ((idx + 3) `div` width) == (idx `div`  width) && grid!(idx + 1) == 'M' && grid!(idx + 2) == 'A' && grid!(idx + 3) == 'S'
bot height width grid idx = grid!idx == 'X' && idx + width * 3 < height * width && grid!(idx + width) == 'M' && grid!(idx + width * 2) == 'A' && grid!(idx + width * 3) == 'S'
left height width grid idx = grid!idx == 'X' && ((idx - 3) `div` width) == (idx `div`  width) && grid!(idx - 1) == 'M' && grid!(idx - 2) == 'A' && grid!(idx - 3) == 'S'
topLeft height width grid idx = grid!idx == 'X' && ((idx - 3) `div` width) == (idx `div`  width) && idx - width * 3 >= 0 && grid!(idx - width - 1) == 'M' && grid!(idx - width * 2 - 2) == 'A' &&  grid!(idx - width * 3 - 3) == 'S'
topRight height width grid idx = grid!idx == 'X' && ((idx + 3) `div` width) == (idx `div`  width) && idx - width * 3 >= 0 && grid!(idx - width + 1) == 'M' && grid!(idx - width * 2 + 2) == 'A' &&  grid!(idx - width * 3 + 3) == 'S'
botLeft height width grid idx = grid!idx == 'X' && ((idx - 3) `div` width) == (idx `div`  width) && idx + width * 3 < height * width && grid!(idx + width - 1) == 'M' && grid!(idx + width * 2 - 2) == 'A' &&  grid!(idx + width * 3 - 3) == 'S'
botRight height width grid idx = grid!idx == 'X' && ((idx + 3) `div` width) == (idx `div`  width) && idx + width * 3 < height * width && grid!(idx + width + 1) == 'M' && grid!(idx + width * 2 + 2) == 'A' &&  grid!(idx + width * 3 + 3) == 'S'

sumXMas (height, width, grid) = let
  sumXMas' grid idx
    | idx == height * width = 0
    | otherwise = length (filter id $ map (\ func -> func height width grid idx) [top, right, bot, left, topLeft, topRight, botLeft, botRight]) + sumXMas' grid (idx + 1)
  in sumXMas' grid 0

main =
  getContents
    >>=  print . sumXMas . listToArray