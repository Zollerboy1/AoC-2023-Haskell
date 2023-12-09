import Control.Monad (join)
import Data.Bool (bool)

solve :: [Int] -> Int
solve = join $ bool ((+) <$> head <*> solve . (zipWith (-) <$> init <*> tail)) head . all (==0)

main :: IO ()
main = do
    input <- map (map read . words) . lines <$> readFile "day9.txt"
    print . sum . map (solve . reverse) $ input
    print . sum . map solve $ input
