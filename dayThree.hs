import Data.List.Split
import Data.List
import Data.Set 
import Data.Either.Unwrap

import Text.Parsec
import Text.Parsec.String


main = do
  input <- readFile inputLocation
  let splitInput = lines input
  let fabricLines = fmap (parse parser "") splitInput
  let validFabrics = fmap fromRight fabricLines
  print $ countOverlaps validFabrics
  print $ findSolo validFabrics

inputLocation = "../Downloads/input"

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

int :: Parser Int
int = read <$> many1 digit

parser :: Parser Fabric
parser = do
  _ <- Text.Parsec.oneOf "#"
  line <- int
  _ <- Text.Parsec.oneOf " "
  _ <- Text.Parsec.oneOf "@"
  _ <- Text.Parsec.oneOf " "
  left <- int
  _ <- Text.Parsec.oneOf ","
  top <- int
  _ <- Text.Parsec.oneOf ":" -- TODO Learn how to have it ignore the entire string, not just the one character
  _ <- Text.Parsec.oneOf " "
  width <- int
  _ <- Text.Parsec.oneOf "x"
  height <- int
  return (Fabric line left top width height)
