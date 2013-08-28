module Main(main) where

import System.Exit
import qualified Data.Map.Lazy as Map
import Data.List(intercalate)

type ID = Int
type TrainID = Int
type StationID = Int
type Name = String
type Trains = Map.Map ID Train
type Stations = Map.Map ID Station

data Seat = Seat ID [(Station,Station)] deriving (Show,Read)
data Car = Car ID TrainID [Seat] deriving (Show, Read)
data Station = Station Name [StationID] | NilStation deriving (Show, Read)
data Train = Train Name [Car] [StationID] | NilTrain deriving (Show, Read)

type ReservationData = (Trains,Stations)

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
		"print_stations_v" -> printStations' ss
		"print_trains" -> printTrains ts ss
		"quit" -> quit (ts,ss)
		_ -> usage
	handleInput (ts,ss)

loadData :: IO ReservationData
loadData = do d <- readFile "data.txt"
              return (read d)

usage :: IO()
usage = do
	putStrLn "help/?: Print this text"
	putStrLn "print_stations: Display list of stations"
	putStrLn "print_stations_v: Display list of stations including connected stations"
	putStrLn "print_trains: Display list of trains"
	putStrLn "quit: Quit program"

quit :: ReservationData -> IO()
quit resData = do
	writeFile "data.txt" (show resData)
	exitSuccess

printStations :: Stations -> IO()
printStations ss
	| Map.null ss = putStrLn "No stations found"
	| otherwise = putStrLn $ "Stations: "++stationNames
		where stationNames = intercalate "," $ getStationNames (map (getStationByID ss) stationIDs);
				stationIDs = map fst stations;
				  stations = Map.toList ss

printStations' :: Stations -> IO()
printStations' ss
	| Map.null ss = putStrLn "No stations found"
	| otherwise = do
		putStrLn ""
		putStrLn $ init $ unlines (map (printStation' ss) (Map.toAscList ss))

printStation' :: Stations -> (ID,Station) -> String
printStation' ss (_,NilStation) = "NilStation should not occur!\n"
printStation' ss (i,Station n others) = "Station #"++show i++" '"++n++"' connected with:\n"++(unlines otherNames)
	where otherNames = map ("\t"++) $ getStationNames (map (getStationByID ss) others)
		  

getStationByID :: Stations -> ID -> Station
getStationByID ss i
    | Map.member i ss = ss Map.! i
	| otherwise = NilStation

getStationNames :: [Station] -> [String]
getStationNames [] = []
getStationNames (NilStation:ss) = ["NilStation"] ++ getStationNames ss
getStationNames ((Station n _):ss) = [n] ++ getStationNames ss

printTrains :: Trains -> Stations -> IO()
printTrains ts ss
	| Map.null ts = putStrLn "No trains found.."
	| otherwise = do
		putStrLn ""
		putStrLn $ init $ unlines (map (printTrain ss) (Map.toAscList ts))

printTrain :: Stations -> (ID,Train) -> String
printTrain ss (_,NilTrain) = "NilTrain should not occur!\n"
printTrain ss (i,Train n cs route) = "Train #"++show i++" '"++n++"'\n"++carString++"\n"++routeString++"\n"
	where 	  carString = if null cs then "\t No cars" else "\tCars: "++carInfos;
			   carInfos = intercalate "," (map carInfo cs);
			routeString = if null route then "\tNo stations" else "\tRoute: "++stationNames;
		   numberOfCars = show $ length cs;
		   stationNames = intercalate "," $ getStationNames (map (getStationByID ss) route)

carInfo :: Car -> String
carInfo (Car i _ ss) = "#"++show i++" ("++show (length ss)++" seats)"
