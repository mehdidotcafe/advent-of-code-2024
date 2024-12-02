{-# LANGUAGE TupleSections #-}

import Data.List (map)
import Data.Map (fromListWith, intersectionWith, empty, foldrWithKey)

tuplify [] = error "Empty list"
tuplify [_] = error "Need two elements in array to construct tuple"
tuplify (x:y:_) = (x, y)

toCounter l = fromListWith (+) $ map (,1) l

calc p = let
  l = toCounter $ map fst p
  r = toCounter $ map snd p
  in foldrWithKey  (\ k v acc -> acc + k * v) 0 $ intersectionWith (*) l r
main =
  getContents
    >>=  print . calc . map (tuplify . (map (read :: String -> Int) . words)) . lines
