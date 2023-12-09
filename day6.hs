import Control.Arrow ((***), (&&&))
import Control.Monad (join)
import Data.Bifunctor (second)
import Data.Char (isDigit)
import Data.Function (on)

splitOnFirst :: Eq a => a -> [a] -> ([a], [a])
splitOnFirst = (second (drop 1) .) . break . (==)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = ((filter (not . null) . map fst . takeWhile (not . uncurry ((&&) `on` null))) .) . (. ([],)) . iterate . ((. snd) . splitOnFirst)

solve :: String -> Int
solve = product . uncurry (zipWith (((pred . uncurry (-)) .) . ((.) <$> ((&&&) <$> (ceiling .) . (+) <*> (floor .) . (-)) . (/2) <*> (sqrt .) . (-) . (/4) . (^^2)))) . (join (***) $ map read . splitOn ' ' . dropWhile (not . isDigit)) . break (=='\n')

main :: IO ()
main = do
    input <- readFile "day6.txt"
    print . solve $ input
    print . solve . filter (/=' ') $ input
