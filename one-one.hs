import Data.List.Split
import Data.List
import Data.Set 

oldMain = do
  input <- readFile inputLocation
  let splitInput = fmap parse $ splitOn "\n" input 
      frequencies = fmap ignoreNothing splitInput
  print frequencies
  print $ defaultCall frequencies
  print $ defaultCall [-1, 1]

main = do
  input <- readFile inputLocation
  let splitInput = splitOn "\n" input
  print splitInput
  let checksums = fmap checksum splitInput
  let bti x = if x == True then 1 else 0
  let (x,y) = (sum (fmap (bti . fst) checksums), sum (fmap (bti . snd) checksums))
  print (x*y)
  print $ areSimilar "fghij" "fguij" False 
  print $ areSimilar "fghij" "fguil" False
  print $ Data.List.filter (\(_,_,x) -> x == True) [(x, y, areSimilar x y False) | x <- splitInput, y <- splitInput]



areSimilar :: String -> String -> Bool -> Bool
areSimilar [] [] areDifferent = if areDifferent then True else False
areSimilar (x:xs) [] _ = False
areSimilar [] (y:ys) _ = False
areSimilar (x:xs) (y:ys) foundDifferent = if x == y then areSimilar xs ys foundDifferent else if foundDifferent == True then False else areSimilar xs ys True


checksum :: String -> (Bool, Bool)
checksum x = (elem 2 finalCounts , elem 3 finalCounts)
  where finalCounts = fmap length (group sorted)
        sorted = sort x :: String


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

  
inputLocation = "../Downloads/input"
