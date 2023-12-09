import Data.Bits (shift)
import Data.List (intersect)

parse :: String -> [Int]
parse = map (length . uncurry intersect . break (=="|") . words . dropWhile (/= ':')) . lines

part1 :: String -> Int
part1 = sum . map (shift 1 . pred) . parse

part2 :: String -> Int
part2 = snd . foldl (curry $ (,) <$> (zipWith (+) <$> (++ repeat 0) . (replicate <$> snd <*> head . fst .fst) <*> tail . fst . fst) <*> uncurry (flip $ const $ uncurry $ (+) . head)) (repeat 1, 0) . parse

main :: IO ()
main = do
    input <- readFile "day4.txt"
    print . part1 $ input
    print . part2 $ input
