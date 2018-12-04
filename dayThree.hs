import Data.List.Split
import Data.List
import Data.Set 
import Data.Either.Unwrap

import Text.Parsec
import Text.Parsec.String

import Common

main = do
  input <- readFile inputLocation
  let splitInput = lines input
  let fabricLines = fmap (parse parser "") splitInput
  let validFabrics = fmap fromRight fabricLines
  print ("The answer to part one is " ++ (show (countOverlaps validFabrics)))
  print ("The answer to part two is " ++ (show (lineNo $ findSolo validFabrics)))

type Coord = (Int, Int)

data Fabric = Fabric {
  lineNo :: Int,
  leftOffset :: Int,
  topOffset :: Int,
  width :: Int,
  height :: Int } deriving Show


fabricToCoords :: Fabric -> [Coord]
fabricToCoords (Fabric line left top wi hei) = [(x,y) | x <- [left+1 .. left + wi], y <- [top+1 .. top + hei]]


countOverlaps :: [Fabric] -> Int
countOverlaps fabrics = length $ Data.List.filter (\x -> x >= 2) $ fmap snd $ getCoordCounts fabrics


findSolo :: [Fabric] -> Fabric
findSolo fabrics = head $ take 1 $ Data.List.filter (\x -> subset (fabricToCoords x) onceUsedSpots) fabrics
  where onceUsedSpots = fmap fst $ Data.List.filter (\x -> snd x == 1) $ getCoordCounts fabrics :: [Coord]


subset xs ys  = all (`elem` ys) xs 

getCoordCounts :: [Fabric] -> [(Coord, Int)]
getCoordCounts fabrics = coordCounts 
  where coords = concat $ fmap fabricToCoords fabrics
        groupedCoords = (group . sort) coords 
        coordCounts = fmap (\x -> (x !! 0, length x)) groupedCoords 


parser :: Parser Fabric
parser = do
  _ <- Text.Parsec.string "#"
  line <- int
  _ <- Text.Parsec.string " @ "
  left <- int
  _ <- Text.Parsec.oneOf ","
  top <- int
  _ <- Text.Parsec.string ": "
  width <- int
  _ <- Text.Parsec.oneOf "x"
  height <- int
  return (Fabric line left top width height)


inputLocation = "../Downloads/input-3.txt"
