import System.IO
import Data.List.Split
import qualified Data.Set as Set
import Control.Monad

main :: IO [()]
main = do
  input  <- readFile  "A-small-practice.in" 
  -- rewrite splitOn " " with lambda?
  let lines = map listToInt $ map (splitOn " ") $ splitOn "\n" $ init input 

  let numberOfTestCases = head $ head lines
  let testCases = tail lines 

  processTestCases numberOfTestCases testCases 
  -- can't do this cause `print $ processTestCases numberOfTestCases testCases` returns sth funny
  --writeFile "output.txt" $ foldl (\acc str -> str ++ "\n") "" $ processTestCases numberOfTestCases testCases 

listToInt :: [String] -> [Int]
listToInt str = map read str

parseLine :: Int -> [[Int]] -> [[Int]]
parseLine index (x:xs) 
  | index == 1 = x:xs
  | otherwise = parseLine (index - 1) xs 

tailLines :: Int -> [[Int]] -> [[Int]]
tailLines index lines 
  | index == 0 = lines
  | index == 1 = tail lines 
  | otherwise = tailLines (index - 1) (tail lines) 

getIntersection :: [[Int]] -> Set.Set Int 
getIntersection lines = do
  let where_ans_is = head $ head lines
  let first  = Set.fromList $ (parseLine where_ans_is (tail lines)) !! 0

  let tails = tailLines 5 lines
  let where_ans_is' = head $ head tails
  let second = Set.fromList $ (parseLine where_ans_is' (tail tails)) !! 0

  Set.intersection first second

processTestCases :: Int -> [[Int]] -> IO [()] 
processTestCases numberOfTestCases testCases = do
  forM [0..numberOfTestCases - 1] $ \i -> do
    appendFile "A-small-practice.out" $ "Case #" ++ (show (i + 1)) ++ ": " ++ (setText $ getIntersection (tailLines (i * 10)  testCases)) ++ "\n"

setText :: Set.Set Int -> String 
setText intersection
  | Set.size intersection == 0 = "Volunteer cheated!"
  | Set.size intersection == 1 = selectOneFrom intersection 
  | otherwise = "Bad magician!"

selectOneFrom :: Set.Set Int -> String
selectOneFrom set = show $ head $ Set.toList set
