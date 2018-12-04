import Text.Parsec
import Text.Parsec.String

import Common

main = do
  input <- fmap lines $ readFile inputLocation
  print $ fmap (parse parser "") input
  print ("The solution to part one is " ++ "")
  print ("The solution to part two is " ++ "")

data Timestamp = Timestamp {
  year :: Int,
  month :: Int,
  day :: Int,
  hour :: Int,
  minute :: Int,
  action :: String } deriving Show


parser :: Parser Timestamp
parser = do
  _ <- Text.Parsec.oneOf "["
  year <- int
  _ <- Text.Parsec.oneOf "-"
  month <- int
  _ <- Text.Parsec.oneOf "-"
  day <- int
  _ <- Text.Parsec.oneOf " "
  hour <- int
  _ <- Text.Parsec.oneOf ":"
  minute <- int
  _ <- Text.Parsec.oneOf "]"
  action <- many anyChar
  
  return (Timestamp year month day hour minute action)




inputLocation = "../Downloads/input-4.txt"
