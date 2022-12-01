import Data.List (sortOn)

main :: IO ()
main = do
  inp <- lines <$> readFile "inp/day1.txt"
  putStr "Day1: Part1: "
  print $ maxElfCalories inp
  let elfList = sortOn (\x -> -x) $ sumElves inp
  putStr "Day1: Part1 (alt): "
  print $ head elfList
  putStr "Day1: Part2: "
  print $ sum $ take 3 elfList

maxElfCalories :: [String] -> Int
maxElfCalories ls = helper 0 0 ls
  where
    helper best acc [] = best
    helper best acc ("":xs) = helper (max best acc) 0 xs
    helper best acc (x:xs) = helper best (acc + (read x)) xs

sumElves :: [String] -> [Int]
sumElves = go 0
  where
    go :: Int -> [String] -> [Int]
    go acc [] = [acc]
    go acc ("":xs) = acc : (go 0 xs)
    go acc (x:xs) = go (acc + (read x)) xs
