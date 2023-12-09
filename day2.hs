import Data.Bifunctor (bimap, first, second)
import Data.Function (on)
import Data.Tuple (swap)

splitOnFirst :: Eq a => a -> [a] -> ([a], [a])
splitOnFirst = (second (drop 1) .) . break . (==)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = ((filter (not . null) . map fst . takeWhile (not . uncurry ((&&) `on` null))) .) . (. ([],)) . iterate . ((. snd) . splitOnFirst)

data Game = Game {
    maxRed :: Int,
    maxGreen :: Int,
    maxBlue :: Int,
    gameID :: Int
} deriving (Show)

maxcolor :: String -> [(Int, String)] -> Int
maxcolor = ((maximum . map fst) .) . filter . ((. snd) . (==))

parse :: String -> Game
parse = uncurry (uncurry (uncurry Game)) . swap . bimap (read . drop 5) (((,) <$> ((,) <$> maxcolor "red" <*> maxcolor "green") <*> maxcolor "blue") . map (first read . splitOnFirst ' ') . concatMap (map tail . splitOn ',') . splitOn ';') . splitOnFirst ':'

part1 :: [Game] -> Int
part1 = sum . map gameID . filter ((&&) <$> ((&&) <$> (<=12) . maxRed <*> (<=13) . maxGreen) <*> (<=14) . maxBlue)

part2 :: [Game] -> Int
part2 = sum . map ((*) <$> ((*) <$> maxRed <*> maxGreen) <*> maxBlue)

main :: IO ()
main = do
    games <- map parse . lines <$> readFile "day2.txt"
    print . part1 $ games
    print . part2 $ games
