import qualified Data.List.Split as LS (splitOn)
import qualified Data.List as L (isPrefixOf)

parse l = let
  [towels, designs] = LS.splitOn [""] $ lines l
  in (map (dropWhile (== ' ')) $ LS.splitOn "," $ head towels, designs)

solve towels designs = let
  solve' design
    | null design = True
    | otherwise = any (solve' . flip drop design . length) $ filter (flip L.isPrefixOf design) towels
  in length $ filter id $ map solve' designs

main =
  getContents
    >>=  print . (uncurry solve) . parse 
