import System.IO
import Data.List.Split
import Control.Monad

main :: IO [()]
main = do
  input <- readFile "B-large-practice.in" 

  let lines = map listToDouble $ map (splitOn " ") $ splitOn "\n" $ init input 
  let numberOfTestCases = floor $ head $ head lines
  let testCases = tail lines 

  processTestCases numberOfTestCases testCases

processTestCases :: Int -> [[Double]] -> IO [()] 
processTestCases numberOfTestCases testCases = do
  forM [0..numberOfTestCases - 1] $ \i -> do
    let testCase = testCases !! i 
    let c = testCase !! 0 
    let f = testCase !! 1 
    let x = testCase !! 2 
    appendFile "B-large-practice.out" $ "Case #" ++ (show (i + 1)) ++ ": " ++ (calcTotalCost (masterFormula x f) $ zip indexList $ [0] ++ scanl1 (+) (map (costForFarm c f) indexList)) ++ "\n"
    --appendFile "output.txt" $ "Case #" ++ (show (i + 1)) ++ ": " ++ (calcTotalCost (masterFormula x f) $ zip indexList $ [0] ++ scanl1 (+) (map (costForFarm c f) indexList)) ++ "\n"

listToDouble :: [String] -> [Double]
listToDouble str = map read str

indexList :: [Double]
indexList = [0..]

masterFormula :: Double -> Double -> Double -> Double
masterFormula x f n = (x / (f * n + 2))

calcTotalCost :: (Double -> Double) -> [(Double, Double)] -> String 
calcTotalCost formula (x:y:xs)
  | ((snd x) + formula (fst x)) >= ((snd y) + formula (fst y)) = calcTotalCost formula (y:xs)
  | ((snd x) + formula (fst x)) < ((snd y) + formula (fst y)) = show $ (snd x) + formula (fst x) 

costForFarm :: Double -> Double -> Double -> Double
costForFarm c f n = c / (f * n + 2)

sumOfCostForFarm :: Double -> Double -> Double -> Double -> Double -> Double
sumOfCostForFarm c f x n accmulator
  | n == 1 = accmulator + costForFarm c f n 
  | otherwise = sumOfCostForFarm c f x n $ costForFarm c f n 
