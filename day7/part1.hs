

parse = map ( (\list -> (head list, tail list)) . map (read :: String -> Int)) . map (words . filter (/= ':')) . lines

eval (toFind, numbers) = let
  eval' [] sum = toFind == sum 
  eval' (x:xs) sum = eval' xs (x + sum) || eval' xs (x * sum)
  in eval' (tail numbers) (head numbers)

main =
  getContents
    >>=  print . foldr ((+) . fst) 0 . filter (id . snd) . map (\ l -> (fst l, eval l)) . parse
