module Main(main) where

import System.Exit

type ID = Int
type TrainID = Int
type StationID = Int
type Name = String

data Seat = Seat [(Station,Station)] deriving (Show,Read)
data Car = Car ID TrainID [Seat] deriving (Show, Read)
data Station = Station ID Name [StationID] deriving (Show, Read)
data Train = Train ID Name [Car] [StationID] deriving (Show, Read)

type ReservationData = ([Train],[Station])

main :: IO()
main = do resData <- loadData
          putStrLn "Welcome to Train Reservation Manager. Press ? or enter 'help' for a quick introduction"
          handleInput resData

handleInput :: ReservationData -> IO()
handleInput (ts,ss) = do
	putStrLn ">> "
	input <- getLine
	case ( head (words input) ) of
		"help" -> usage
		"?" -> usage
		"print_stations" -> printStations ss
		"print_trains" -> printTrains ts ss
		"quit" -> quit
		_ -> usage
	handleInput (ts,ss)

loadData :: IO ReservationData
loadData = do d <- readFile "data.txt"
              return (read d)

usage :: IO()
usage = do
	putStrLn "help/?: Print this text"
	putStrLn "print_stations: Display list of stations"
	putStrLn "print_trains: Display list of trains"
	putStrLn "quit: Quit program"

quit :: IO()
quit = exitSuccess

printStations :: [Station] -> IO()
printStations [] = putStr ""
printStations (s:ss) = do
	print s
	printStations ss

printTrains :: [Train] -> [Station] -> IO()
printTrains [] ss = putStr ""
printTrains (t:ts) ss = do
	print t
	printTrains ts ss

testData :: ReservationData
testData = ([(Train 1 "Testzug 1" [(Car 1 1 []), (Car 2 1 [])] [1,2,3])],[(Station 1 "Wien West" [2]), (Station 2 "St. Poelten" [1,3]), (Station 3 "Salzburg" [2])])
