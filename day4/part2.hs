import Data.Array (listArray, (!))

listToArray list = let
  l = lines list
  height = length l
  width = length $ head l
  in (height, width, listArray (0, width * height - 1) (concat l))

isContainedInArray height width idx grid = idx > width && idx < height * (width - 1) - 1 && idx `mod` width >= 1 && idx `mod` width < width - 1

isMas x1 x2 grid = (grid!x1 == 'S' && grid!x2 == 'M') || (grid!x1 == 'M' && grid!x2 == 'S')

isXmas height width idx grid = isContainedInArray height width idx grid && grid!idx == 'A'
  && isMas (idx - width - 1) (idx + width + 1) grid
  && isMas (idx - width + 1) (idx + width - 1) grid

sumXMas (height, width, grid) = let
  sumXMas' grid idx
    | idx == height * width = 0
    | otherwise = (if isXmas height width idx grid then 1 else 0) + sumXMas' grid (idx + 1)
  in sumXMas' grid 0

main =
  getContents
    >>=  print . sumXMas . listToArray