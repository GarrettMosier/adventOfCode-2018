import Data.List.Split
import Data.List
import Data.Set 

main = do
  input <- readFile inputLocation
  let splitInput = fmap parse $ lines input 
      frequencies = fmap (maybe 0 id) splitInput
  print ("The answer to part one is " ++ show (sum frequencies))
  print ("The answer to part two is " ++ show (findDuplicate frequencies))


findDuplicate :: [Int] -> Maybe Int
findDuplicate x = findDuplicateHelper empty (cycle x) 0


findDuplicateHelper :: Set Int -> [Int] -> Int -> Maybe Int
findDuplicateHelper _ [] _ = Nothing
findDuplicateHelper foundFrequencies (currentChange:changes) oldFrequency = if member newFrequency foundFrequencies
  then return newFrequency
  else findDuplicateHelper updatedFoundFrequencies changes newFrequency

  where newFrequency = oldFrequency + currentChange
        updatedFoundFrequencies = Data.Set.insert oldFrequency foundFrequencies


parse :: String -> Maybe Int
parse (sign:number) = if sign == '-'
  then return $ -1 * (read number)
  else return $ read number
parse x = Nothing

  
inputLocation = "../Downloads/input-1.txt"
