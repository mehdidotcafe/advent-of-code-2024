import qualified Data.Char as C (isDigit)
import qualified Data.List.Split as LS (splitOn)
import qualified Data.Map.Strict as M (fromList, (!))
import qualified Data.List as L (intercalate, isSuffixOf, (!?), uncons, sort, find)
import qualified Data.Bits as B (xor)
import qualified Data.Maybe as MA (catMaybes)

parse l = let
  extract = (read :: String -> Int) . dropWhile (not . C.isDigit)
  [[registerA, registerB, registerC], [program]] = LS.splitOn [""] $ lines l
  in ((0, [], 1, extract registerB, extract registerC), map (read :: String -> Int ) $ LS.splitOn "," $ dropWhile (not . C.isDigit) program)


opcodes = M.fromList [(0, adv), (1, bxl), (2, bst), (3, jnz), (4, bxc), (5, out), (6, bdv), (7, cdv)]

combos (_, __, a, b, c) operand
  | operand == 4 = a
  | operand == 5 = b
  | operand == 6 = c
  | otherwise = operand

adv d@(i, p, a, b, c) operand = (i + 2, p, a', b, c)
  where a' = a `div` (2 ^ (combos d operand))

bxl d@(i, p, a, b, c) operand = (i + 2, p, a, b', c)
  where b' = b `B.xor` operand

bst d@(i, p, a, b, c) operand = (i + 2, p, a, b', c)
  where b' = (combos d operand) `mod` 8 

jnz d@(i, p, a, b, c) operand
  | a == 0 = (i + 2, p, a, b, c)
  | otherwise = (i', p, a, b, c)
  where i' = operand

bxc d@(i, p, a, b, c) operand = (i + 2, p, a, b', c)
  where b' = b `B.xor` c

out d@(i, p, a, b, c) operand = (i + 2, p ++ [o], a, b, c)
  where o = (combos d operand) `mod` 8

bdv d@(i, p, a, b, c) operand = (i + 2, p, a, b', c)
  where b' = a `div` (2 ^ (combos d operand))

cdv d@(i, p, a, b, c) operand = (i + 2, p, a, b, c')
  where c' = a `div` (2 ^ (combos d operand))

getProgram (_, p, _, _, _) = p

solve (i, p, a, b, c) program = let
  solve' d@(i, p, a, b, c) = case (program L.!? i,program L.!? (i + 1)) of
    (Nothing, _) -> d
    (_, Nothing) -> d
    (Just opcode, Just operand) -> solve' ((opcodes M.! opcode) d $ operand)
  out = map (\ a' -> ((getProgram $ solve' (i, p, a', b, c), a'))) [a..(a + 7)]
  in case L.find ((== program) . fst) out of
    Just (_, a) -> Just $ a
    Nothing -> case L.uncons $ L.sort $ MA.catMaybes $ map ( \ (_, a) -> solve (i, p, a * 8, b, c) program) $ filter (flip L.isSuffixOf program . fst) out of
        Nothing -> Nothing
        Just (a, _) -> Just a

main =
  getContents
    >>=  print . (uncurry solve)  . parse 
