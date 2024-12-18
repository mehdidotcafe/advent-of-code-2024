import qualified Data.List as L (find, foldl', foldr)
import qualified Data.List.Utils as LU (replace)
import qualified Data.List.Split as LS (splitOn)
import qualified Data.Map.Strict as M (fromList, assocs, (!), union, unions, filter, foldrWithKey, singleton, empty, unionWith)
import qualified Data.Maybe as Ma (fromMaybe)

genMaze (height, width) rows = M.fromList $ concatMap (\ (y, l) -> map (\ (x, c) -> ((x, y), c)) l) $ zip [0..] $ map (zip [0..]) rows

getInitialPos board = fst . Ma.fromMaybe ((0, 0), '@') . L.find (\ (k, v) -> v == '@') $ M.assocs board

mVectors = M.fromList [('<', ((-1), 0)), ('^', (0, (-1))), ('>', (1, 0)), ('v', (0, 1))]

mBrackets = M.fromList [('[', (1, 0)), (']', (-1, 0))]

mPushObstacles = M.fromList [('<', tryPushXObstacle), ('^', tryPushYObstacles), ('>', tryPushXObstacle), ('v', tryPushYObstacles)]

addVectors (a, b) (c, d) = (a + c, b + d)

parse input = let
  inputLines = lines input
  [rows, moves] = LS.splitOn [""] inputLines
  wideRows = map (\ row -> L.foldr (\ (a, b) acc -> LU.replace a b acc) row [("@", "@."), ("#", "##"), ("O", "[]"), (".", "..")]) rows
  height = length wideRows
  width = length $ head wideRows
  board = genMaze (height, width) wideRows
  in ((height, width), getInitialPos board, board, concat moves)

pushXObstacles pos vector board
    | siblingTile == '#' = Nothing
    | siblingTile == '[' || siblingTile == ']' = case pushXObstacles siblingTilePos vector board of
        Nothing -> Nothing
        Just m -> Just ((M.singleton siblingTilePos (board M.! pos)) `M.union` m)
    | otherwise = Just (M.singleton siblingTilePos (board M.! pos))
    where 
      siblingTilePos = addVectors pos vector
      siblingTile = board M.! siblingTilePos

tryPushXObstacle pos siblingTilePos vector board = case pushXObstacles siblingTilePos vector board of
  Nothing -> (pos, board)
  Just updatedTiles -> (siblingTilePos, (M.unions [M.singleton siblingTilePos '.', updatedTiles, board]))

pushYObstacles pos vector board
  | siblingTile1 == '#' || siblingTile2 == '#' = Nothing
  | any (flip elem ['[', ']']) [siblingTile1, siblingTile2] = case (updateCoords siblingTilePos1, updateCoords siblingTilePos2) of
      (Nothing, Nothing) -> Nothing
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just a, Just b) -> Just $ M.unionWith (\ a b -> if a == '.' && b /= '.' then b else a) a b 
  | otherwise = Just $ M.fromList [(siblingTilePos1, (board M.! pos)), (siblingTilePos2, (board M.! (addVectors pos bracketVector))), (pos , '.'), (addVectors pos bracketVector, '.')]
  where
    bracketVector = mBrackets M.! (board M.! pos)
    siblingTilePos1 = addVectors pos vector
    siblingTile1 = board M.! siblingTilePos1
    siblingTilePos2 = addVectors siblingTilePos1 bracketVector
    siblingTile2 = board M.! siblingTilePos2
    updateCoords siblingTilePos = if board M.! siblingTilePos `elem` ['[', ']'] then case pushYObstacles siblingTilePos vector board of
        Nothing -> Nothing
        Just m -> Just $ M.unions [M.fromList [(siblingTilePos1, (board M.! pos)), (siblingTilePos2, (board M.! (addVectors pos bracketVector))), (pos , '.'), (addVectors pos bracketVector, '.')], m]
      else Just $ M.empty

tryPushYObstacles pos siblingTilePos vector board = case pushYObstacles siblingTilePos vector board of
  Nothing -> (pos, board)
  Just m -> (siblingTilePos, (M.unions [M.fromList [(siblingTilePos, '.'), (addVectors siblingTilePos bracketVector, '.')], m, board]))
  where
    bracketVector = mBrackets M.! (board M.! siblingTilePos)


solve ((height, width), initialPos, initialBoard, moves) = let
  solve' (pos, board) move
    | siblingTile == '#' = (pos, board)
    | siblingTile == '[' || siblingTile == ']' = tryPush pos siblingTilePos vector board
    | otherwise = (siblingTilePos, board)
    where
      tryPush = mPushObstacles M.! move
      vector = mVectors M.! move
      siblingTilePos = addVectors pos vector
      siblingTile = board M.! siblingTilePos
  in M.foldrWithKey (\ (x, y) _ acc -> acc + (100 * y) + x) 0 $ M.filter (== '[') $ snd $ L.foldl' solve' (initialPos, initialBoard) moves

main =
  getContents
    >>=  print  . solve . parse 
