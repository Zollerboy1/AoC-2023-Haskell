import Control.Arrow ((***), (&&&))
import Control.Monad (join)
import Data.Char (isDigit)

solve :: String -> Int
solve = product . uncurry (zipWith (((pred . uncurry (-)) .) . ((.) <$> ((&&&) <$> (ceiling .) . (+) <*> (floor .) . (-)) . (/2) <*> (sqrt .) . (-) . (/4) . (^^2)))) . (join (***) $ map read . words . dropWhile (not . isDigit)) . break (=='\n')

main :: IO ()
main = do
    input <- readFile "day6.txt"
    print . solve $ input
    print . solve . filter (/=' ') $ input
