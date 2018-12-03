import Data.List.Split
import Data.List
import Data.Set 


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

inputLocation = "../Downloads/input-2.txt"
