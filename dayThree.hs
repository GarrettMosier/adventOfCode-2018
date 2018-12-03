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
  print fabricLines
  print $ countOverlaps $ fmap fromRight fabricLines

inputLocation = "../Downloads/input"

type Coord = (Int, Int)

data Fabric = Fabric {
  leftOffset :: Int,
  topOffset :: Int,
  width :: Int,
  height :: Int } deriving Show

fabricToCoords :: Fabric -> [Coord]
fabricToCoords (Fabric left top wi hei) = [(x,y) | x <- [left+1 .. left + wi], y <- [top+1 .. top + hei]]

countOverlaps :: [Fabric] -> Int
countOverlaps fabrics = length $ Data.List.filter (\x -> x >= 2) coordCounts
  where coords = concat $ fmap fabricToCoords fabrics
        groupedCoords = (group . sort) coords 
        coordCounts = fmap length groupedCoords 

int :: Parser Int
int = read <$> many1 digit

parser :: Parser Fabric
parser = do
  _ <- Text.Parsec.oneOf "#"
  _ <- int
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
  return (Fabric left top width height)
  



{-
parseFabric :: String -> Coord
parseFabric x = 
  where dimensions = splitOn "@" x
        otherDimensions = splitOn ":" dimensions
        offsets = split "," (otherDimensions !! 0)
        position = split ":" (otherDimensions !! 1)
-}
