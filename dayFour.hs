main = do
  input <- fmap lines $ readFile inputLocation
  print input

inputLocation = "../Downloads/input-4.txt"