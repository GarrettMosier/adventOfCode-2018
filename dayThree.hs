import Data.List.Split
import Data.List
import Data.Set 


main = do
  input <- readFile inputLocation
  let splitInput = splitOn "\n" input
  print splitInput

inputLocation = "../Downloads/input"

type Coord = (Int, Int)

data Fabric = Fabric {
  leftOffset :: Int,
  topOffset :: Int,
  width :: Int,
  height :: Int } deriving Show

fabricToCoords :: Fabric -> [Coord]
fabricToCoords (Fabric left top wi hei) = [(x,y) | x <- [left .. left + wi], y <- [top .. top + hei]]

countOverlaps :: [Fabric] -> Int
countOverlaps fabrics = length $ Data.List.filter (\x -> x >= 2) coordCounts
  where coords = concat $ fmap fabricToCoords fabrics
        groupedCoords = (group . sort) coords 
        coordCounts = fmap length groupedCoords 

{-
parseFabric :: String -> Coord
parseFabric x = 
  where dimensions = splitOn "@" x
        otherDimensions = splitOn ":" dimensions
        offsets = split "," (otherDimensions !! 0)
        position = split ":" (otherDimensions !! 1)
-}
