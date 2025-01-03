import qualified Data.Map.Strict as M (fromList, (!))
import qualified Data.Char as C (isDigit)
import qualified Data.List as L (dropWhileEnd, permutations, nub)
import qualified Data.Set as S (fromList, notMember)

a = [(('A', '0'), "<"), (('A', '1'), "^<<"), (('A', '2'), "^<"), (('A', '3'), "^"), (('A', '4'), "^^<<"), (('A', '5'), "^^<"), (('A', '6'), "^^"), (('A', '7'), "^^^<<"), (('A', '8'), "^^^<"), (('A', '9'), "^^^")]
zero = [(('0', 'A'), ">"), (('0', '1'), "^<"), (('0', '2'), "^"), (('0', '3'), "^>"), (('0', '4'), "^^<"), (('0', '5'), "^^"), (('0', '6'), "^^>"), (('0', '7'), "^^^<"), (('0', '8'), "^^^"), (('0', '9'), "^^^>")]
one = [(('1', 'A'), ">>v"), (('1', '0'), ">v"), (('1', '2'), ">"), (('1', '3'), ">>"), (('1', '4'), "^"), (('1', '5'), "^>"), (('1', '6'), "^>>"), (('1', '7'), "^^"), (('1', '8'), "^^>"), (('1', '9'), "^^>>")]
two = [(('2', 'A'), "v>"), (('2', '0'), "v"), (('2', '1'), "<"), (('2', '3'), ">"), (('2', '4'), "^<"), (('2', '5'), "^"), (('2', '6'), "^>"), (('2', '7'), "^^<"), (('2', '8'), "^^"), (('2', '9'), "^^>")]
three = [(('3', 'A'), "v"), (('3', '0'), "v<"), (('3', '1'), "<<"), (('3', '2'), "<"), (('3', '4'), "^<<"), (('3', '5'), "^<"), (('3', '6'), "^"), (('3', '7'), "^^<<"), (('3', '8'), "^^<"), (('3', '9'), "^^")]
four = [(('4', 'A'), ">>vv"), (('4', '0'), ">vv"), (('4', '1'), "v"), (('4', '2'), "v>"), (('4', '3'), "v>>"), (('4', '5'), ">"), (('4', '6'), ">>"), (('4', '7'), "^"), (('4', '8'), "^>"), (('4', '9'), "^>>")]
five = [(('5', 'A'), ">vv"), (('5', '0'), "vv"), (('5', '1'), "v<"), (('5', '2'), "v"), (('5', '3'), "v>"), (('5', '4'), "<"), (('5', '6'), ">"), (('5', '7'), "^<"), (('5', '8'), "^"), (('5', '9'), "^>")]
six = [(('6', 'A'), "vv"), (('6', '0'), "vv<"), (('6', '1'), "v<<"), (('6', '2'), "v<"), (('6', '3'), "v"), (('6', '4'), "<<"), (('6', '5'), "<"), (('6', '7'), "^<<"), (('6', '8'), "^<"), (('6', '9'), "^")]
seven = [(('7', 'A'), ">>vvv"), (('7', '0'), ">vvv"), (('7', '1'), "vv"), (('7', '2'), "vv>"), (('7', '3'), "vv>>"), (('7', '4'), "v"), (('7', '5'), "v>"), (('7', '6'), "v>>"), (('7', '8'), ">"), (('7', '9'), ">>")]
eight = [(('8', 'A'), ">vvv"), (('8', '0'), "vvv"), (('8', '1'), "vv<"), (('8', '2'), "vv"), (('8', '3'), "vv>"), (('8', '4'), "v<"), (('8', '5'), "v"), (('8', '6'), "v>"), (('8', '7'), "<"), (('8', '9'), ">")]
nine = [(('9', 'A'), "vvv"), (('9', '0'), "vvv<"), (('9', '1'), "vv<<"), (('9', '2'), "vv<"), (('9', '3'), "vv"), (('9', '4'), "v<<"), (('9', '5'), "v<"), (('9', '6'), "v"), (('9', '7'), "<<"), (('9', '8'), "<")]

nkp = M.fromList $ map (\ (k, v) -> (k,  L.nub $ L.permutations $ v)) $ a ++ zero ++ one ++ two ++ three ++ four ++ five ++ six ++ seven ++ eight ++ nine

nkpe = S.fromList [('0', "<^^"), ('0', "<^"), ('0', "<^^^"), ('A', "<<^^"), ('A', "<<^"), ('A', "<<^^^"), ('1', "v>"), ('1', "v>>"), ('4', "vv>>"), ('A', "vv>")]

da = [(('A', '^'), "<"), (('A', '>'), "v"), (('A', 'v'), "v<"), (('A', '<'), "v<<"), (('A', 'A'), "")]
up = [(('^', 'A'), ">"), (('^', '>'), "v>"), (('^', 'v'), "v"), (('^', '<'), "v<"), (('^', '^'), "")]
right = [(('>', 'A'), "^"), (('>', '^'), "<^"), (('>', 'v'), "<"), (('>', '<'), "<<"), (('>', '>'), "")]
down = [(('v', 'A'), "^>"), (('v', '^'), "^"), (('v', '>'), ">"), (('v', '<'), "<"), (('v', 'v'), "")]
left = [(('<', 'A'), ">>^"), (('<', '^'), ">^"), (('<', 'v'), ">"), (('<', '>'), ">>"), (('<', '<'), "")]


dkp = M.fromList $ map (\ (k, v) -> (k,  L.nub $ L.permutations $ v)) $ da ++ up ++ right ++ down ++ left

dkpe = S.fromList [('<', "^>"), ('<', "^>>"), ('^', "<v")]

parse = lines

solve codes = let
  computeComplexity code directionLength = (directionLength, numericCode)
    where numericCode = (read :: String -> Int) $ L.dropWhileEnd (not . C.isDigit) code
  solve'' kp kpe button [] = [""]
  solve'' kp kpe button (b:bs) = concatMap (\ d -> map (\ n -> d ++ "A" ++ n) recDirections) directions
    where directions = filter (\ d -> (button, d) `S.notMember` kpe) $ kp M.! (button, b)
          recDirections = solve'' kp kpe b bs
  solve' code = minimum $ map length $ concatMap (solve'' dkp dkpe 'A') $ concatMap (solve'' dkp dkpe 'A') $ solve'' nkp nkpe 'A' code
  in map (\x -> computeComplexity x $ solve' x) codes

main =
  getContents
    >>=  print . solve . parse 
