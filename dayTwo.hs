import Data.List.Split
import Data.List
import Data.Set 

type ID = String

main = do
  input <- (fmap lines (readFile inputLocation))
  let checksums = fmap checksum input
  let bti x = if x then 1 else 0
  let (twos, threes) = (sum (fmap (bti . fst) checksums), sum (fmap (bti . snd) checksums))
  print ("The answer to part one is " ++ (show (twos * threes)))
  print $ findSimilarPairs input


findSimilarPairs :: [ID] -> [(ID, ID, Bool)]
findSimilarPairs fileInput = Data.List.filter (\(_,_,x) -> x) [(x, y, areSimilar x y) | x <- fileInput, y <- fileInput]


areSimilar :: ID -> ID -> Bool
areSimilar x y = areSimilarHelper x y False 

areSimilarHelper :: ID -> ID -> Bool -> Bool
areSimilarHelper [] [] haveFoundDifference = haveFoundDifference
areSimilarHelper (x:xs) [] _ = False
areSimilarHelper [] (y:ys) _ = False
areSimilarHelper (x:xs) (y:ys) haveFoundDifference = if x == y
  then areSimilarHelper xs ys haveFoundDifference
  else
    if haveFoundDifference
      then False
      else areSimilarHelper xs ys True


checksum :: ID -> (Bool, Bool)
checksum x = (elem 2 finalCounts , elem 3 finalCounts)
  where finalCounts = fmap length ((group . sort) x)


inputLocation = "../Downloads/input-2.txt"
