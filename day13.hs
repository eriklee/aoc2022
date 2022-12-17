{-# LANGUAGE InstanceSigs #-}
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Data.List (sort)

main :: IO ()
main = do
  -- let filename = "inp/day13-test.txt"
  let filename = "inp/day13.txt"
  inp <- filter (/= "") . lines <$> readFile filename
  putStr "Day13: Part 1: "
  print . sum . map fst . filter (id . snd) $ zip (enumFrom 1) (checkInOrder inp)

  let div1p = parse "[[2]]"
  let div2p = parse "[[6]]"
  let inp2 = div1p : div2p : map parse inp
  let ordered = zip (enumFrom 1) $ sort inp2
  let div1 = fst . head . filter ((== div1p) . snd) $ ordered
  let div2 = fst . head . filter ((== div2p) . snd) $ ordered
  putStr "Day13: Part 2: "
  print (div1 * div2)


checkInOrder :: [String] -> [Bool]
checkInOrder [] = []
checkInOrder (l:r:xs) = inOrder l r : (checkInOrder xs)

data RoseList =
    RLI Int 
  | RL [RoseList]
  deriving (Show, Eq)

inOrder :: String -> String -> Bool
inOrder l r = (parse l) <= (parse r)

instance Ord RoseList where
  compare :: RoseList -> RoseList -> Ordering
  compare = cmp

cmp :: RoseList -> RoseList -> Ordering
cmp (RLI x) (RLI y) = compare x y
cmp (RL lx) (RL ly) = list_lte lx ly
cmp (RL lx) (RLI y) = list_lte lx [RLI y]
cmp (RLI x) (RL ly) = list_lte [RLI x] ly

list_lte :: [RoseList] -> [RoseList] -> Ordering
list_lte [] [] = EQ
list_lte [] _ = LT
list_lte _ [] = GT
list_lte (x:xs) (y:ys) = case (cmp x y) of
 EQ -> (xs `list_lte` ys)
 GT -> GT
 LT -> LT

parse :: String -> RoseList
parse = fst . head . readP_to_S parseRL

parseRL :: ReadP RoseList
parseRL = list <++ int

int :: ReadP RoseList
int = RLI <$> read <$> munch1 (isDigit)

list :: ReadP RoseList
list = do
  char '['
  xs <- sepBy parseRL (char ',')
  char ']'
  return $ RL xs


