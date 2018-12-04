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
  action :: Action } deriving (Show, Ord, Eq)

data Action = BeginShift GuardID | FallAsleep | WakeUp deriving (Show, Ord, Eq)

type GuardID = Int


calculateSleepyGuard :: [Timestamp] -> GuardID
calculateSleepyGuard x = calculateSleepyGuardHelper x empty


-- Pattern match on action
calculateSleepyGuardHelper :: [Timestamp] -> Map GuardID Int -> GuardID
calculateSleepyGuardHelper (timestamp@(Timestamp _ _ _ _ _ (BeginShift gid)):ts) x = gid


guardAction :: Parser Action
guardAction = choice [wakeUpParser, fallAsleepParser, beginShiftParser]

wakeUpParser = do
  _ <- "wakes up"
  return WakeUp

fallAsleepParser = do
  _ <- "falls asleep"
  return FallAsleep

beginShiftParser = do 
  _ <- string "Guard #"
  guardID <- int
  _ <- string " begins shift"
  return (BeginShift guardID)


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
  action <- guardAction
  
  return (Timestamp year month day hour minute action)




inputLocation = "../Downloads/input-4.txt"
