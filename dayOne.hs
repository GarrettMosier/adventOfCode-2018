import Data.List.Split
import Data.List
import Data.Set 

main = do
  input <- readFile inputLocation
  let splitInput = fmap parse $ splitOn "\n" input 
      frequencies = fmap ignoreNothing splitInput
  print frequencies
  print $ defaultCall frequencies
  print $ defaultCall [-1, 1]


defaultCall :: [Int] -> Maybe Int
defaultCall x = findDuplicate empty (cycle x) 0


findDuplicate :: Set Int -> [Int] -> Int -> Maybe Int
findDuplicate _ [] _ = Nothing
findDuplicate foundFrequencies (currentChange:changes) oldFrequency = if member newFrequency foundFrequencies
  then return newFrequency
  else findDuplicate updatedFoundFrequencies changes newFrequency

  where newFrequency = oldFrequency + currentChange
        updatedFoundFrequencies = Data.Set.insert oldFrequency foundFrequencies


ignoreNothing Nothing = 0
ignoreNothing (Just x) = x


parse :: String -> Maybe Int
parse (sign:number) = if sign == '-'
  then return $ -1 * (read number)
  else return $ read number
parse x = Nothing

  
inputLocation = "../Downloads/input-1.txt"
