isNotInRange x y = abs (x - y) < 1 || abs (x - y) > 3

isReportSafe [x, y]
  | isNotInRange x y = 0
  | otherwise = 1
isReportSafe [_] = 1
isReportSafe [] = 1
isReportSafe (x:y:z:xs)
  | (x >= y && y <= z) || (x <= y && y >= z) = 0
  | isNotInRange x y = 0
  | otherwise = isReportSafe (y:z:xs)

main =
  getContents
    >>=  print . sum . map (isReportSafe . (map (read :: String -> Int) . words)) . lines
