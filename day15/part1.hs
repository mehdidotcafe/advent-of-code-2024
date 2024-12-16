
import qualified Data.List as L (find, foldl')
import qualified Data.List.Split as LS (splitOn)
import qualified Data.Map.Strict as M (fromList, assocs, (!), union, filter, foldrWithKey)
import qualified Data.Maybe as Ma (fromMaybe)

genBoard (height, width) rows = M.fromList $ concatMap (\ (y, l) -> map (\ (x, c) -> ((x, y), c)) l) $ zip [0..] $ map (zip [0..]) rows

getInitialPos board = fst . Ma.fromMaybe ((0, 0), '@') . L.find (\ (k, v) -> v == '@') $ M.assocs board

mVectors = M.fromList [('<', ((-1), 0)), ('^', (0, (-1))), ('>', (1, 0)), ('v', (0, 1))]

parse input = let
  inputLines = lines input
  [rows, moves] = LS.splitOn [""] inputLines
  height = length rows
  width = length $ head rows
  board = genBoard (height, width) rows
  in ((height, width), getInitialPos board, board, concat moves)

findNextFreeTile pos vector board
    | siblingTile == '#' = Nothing
    | siblingTile == 'O' = findNextFreeTile siblingTilePos vector board
    | otherwise = Just siblingTilePos
    where 
      siblingTilePos = (fst pos + (fst vector),  snd pos + (snd vector))
      siblingTile = board M.! siblingTilePos

tryPushObstacle pos siblingTilePos vector board = case findNextFreeTile siblingTilePos vector board of
  Nothing -> (pos, board)
  Just freeTilePos -> (siblingTilePos, (M.fromList [(siblingTilePos, '.'), (freeTilePos, 'O')]) `M.union` board)

solve ((height, width), initialPos, initialBoard, moves) = let
  solve' (pos, board) move
    | siblingTile == '#' = (pos, board)
    | siblingTile == 'O' = tryPushObstacle pos siblingTilePos vector board
    | otherwise = (siblingTilePos, board)
    where
      vector = mVectors M.! move
      siblingTilePos = (fst pos + (fst vector),  snd pos + (snd vector))
      siblingTile = board M.! siblingTilePos
  in M.foldrWithKey (\ (x, y) _ acc -> acc + (100 * y) + x) 0 $ M.filter (== 'O') $ snd $ L.foldl' solve' (initialPos, initialBoard) moves

main =
  getContents
    >>=  print . solve  . parse 
