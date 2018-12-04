import Text.Parsec
import Text.Parsec.String

import Data.List

import Data.Either

import Data.Map

import Common

main = do
  input <- fmap lines $ readFile inputLocation
  let sortedActions = sort $ rights $ fmap (parse parser "") input
  mapM_ print $ take 10 $ sortedActions
  print ("The solution to part one is " ++ "")
  print ("The solution to part two is " ++ "")


data Timestamp = Timestamp {
  year :: Int,
  month :: Int,
  day :: Int,
  hour :: Int,
  minute :: Int,
  action :: String } deriving (Show, Ord, Eq) -- Make it have the Action type

data Action = BeginShift GuardID | FallAsleep | WakeUp 

type GuardID = Int


calculateSleepyGuard :: [Timestamp] -> GuardID
calculateSleepyGuard x = calculateSleepyGuardHelper x empty


calculateSleepyGuardHelper :: [Timestamp] -> Map GuardID Int -> GuardID
calculateSleepyGuardHelper line = undefined


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
