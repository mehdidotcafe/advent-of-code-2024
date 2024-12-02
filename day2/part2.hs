isNotInRange x y = abs (x - y) < 1 || abs (x - y) > 3

isReportSafe [x, y]
  | isNotInRange x y = False
  | otherwise = True
isReportSafe [_] = True
isReportSafe [] = True
isReportSafe (x:y:z:xs)
  | (x >= y && y <= z) || (x <= y && y >= z) = False
  | isNotInRange x y = False
  | otherwise = isReportSafe (y:z:xs)

isReportSafeOrSo xs = any isReportSafe [ take i xs ++ drop (i + 1) xs | i <-[ 0..length xs - 1]]

main =
  getContents
    >>=  print . length . filter (== True) . map (isReportSafeOrSo . (map (read :: String -> Int) . words)) . lines
