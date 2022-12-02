import Control.Monad

main :: IO ()
main = do
  inp <- lines <$> readFile "inp/day2.txt"
  putStr "Day2: Part 1: "
  print . sum $ scoreGame <$> inp
  putStr "Day2: Part 2: "
  print . sum $ scoreGame2 <$> inp

data RPS = Rock | Paper | Scissors
  deriving (Eq)

scoreGame :: String -> Int
scoreGame line =
  let [opS, ourS] = words line
      op = parseOpMove opS
      our = parseOurMove ourS
  in (playPoints our) + (winPoints op our)

scoreGame2 :: String -> Int
scoreGame2 line =
  let [opS, ourS] = words line
      op = parseOpMove opS
      our = parseOurMovePart2 op ourS
  in (playPoints our) + (winPoints op our)

parseOpMove :: String -> RPS
parseOpMove "A" = Rock
parseOpMove "B" = Paper
parseOpMove "C" = Scissors

parseOurMove :: String -> RPS
parseOurMove "X" = Rock
parseOurMove "Y" = Paper
parseOurMove "Z" = Scissors

parseOurMovePart2 :: RPS -> String -> RPS
parseOurMovePart2 op "X" = losesTo op
parseOurMovePart2 op "Y" = op
parseOurMovePart2 op "Z" = beats op

losesTo :: RPS -> RPS
losesTo Rock = Scissors
losesTo Paper = Rock
losesTo Scissors = Paper

beats :: RPS -> RPS
beats Rock = Paper
beats Paper = Scissors
beats Scissors = Rock

winPoints :: RPS -> RPS -> Int
winPoints x y | x == y = 3
winPoints Rock Paper = 6
winPoints Paper Scissors = 6
winPoints Scissors Rock = 6
winPoints _ _ = 0

playPoints :: RPS -> Int
playPoints Rock = 1
playPoints Paper = 2
playPoints Scissors = 3
