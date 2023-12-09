import Data.List (findIndex, isPrefixOf, tails)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

numbers :: [String]
numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

part1 :: [String] -> [[Int]]
part1 = map $ mapMaybe $ readMaybe . pure

part2 :: [String] -> [[Int]]
part2 = map $ map ((`mod` 10) . succ) . mapMaybe ((`findIndex` numbers) . flip isPrefixOf) . init . tails

get :: [[Int]] -> Int
get = sum . map ((+) <$> (*10) . head <*> last)

main :: IO ()
main = do
    input <- lines <$> readFile "day1.txt"
    print . get . part1 $ input
    print . get . part2 $ input
