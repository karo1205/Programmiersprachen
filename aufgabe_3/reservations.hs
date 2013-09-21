module Main where

import System.Exit
import qualified Data.Map.Lazy as Map
import Data.List(intercalate,elemIndices,(\\),nub,delete,group)
import Numeric(readDec)

type ID = Int
type TrainID = ID
type StationID = ID
type CarID = ID
type SeatID = ID
type ReservationID = ID

type Seats = Int
type MinSeats = Int

type From = StationID
type To = StationID
type Name = String

type Trains = Map.Map TrainID Train
type Stations = Map.Map StationID Station
type Reservations = Map.Map TrainID [Reservation]
type Crossings = [StationID]

data Car = Car CarID TrainID Seats  deriving (Show, Read)
data Station = Station Name | NilStation deriving (Show, Read)
data Reservation = Reservation ReservationID CarID [SeatID] From To deriving (Show,Read)
data Train = Train Name [Car] [StationID] MinSeats | NilTrain deriving (Show, Read)

type ReservationData = (Trains,Stations,Crossings,Reservations)

main :: IO()
main = do
        resData <- loadData
        putStrLn "Welcome to Train Reservation Manager. Press ? or enter 'help' for a quick introduction"
        handleInput resData

usage :: IO()
usage = do
        putStrLn "help/?: Print this text"
        putStrLn "print_stations: Display list of stations"
        putStrLn "print_stations_v: Display list of stations including trains stopping there"
        putStrLn "print_trains: Display list of trains"
        putStrLn "print_trains_v: Display list of trains (verbose)"
        putStrLn "print_route: Display train connections serving a certain route"
        putStrLn "quit: Quit program"

handleInput :: ReservationData -> IO()
handleInput (ts,ss,cs,rs) = do
    putStrLn ">> "
    input <- getLine
    case ( head (words input) ) of
        "help" -> usage
        "?" -> usage
        "print_stations" -> printStations ss ts
        "print_stations_v" -> printStations' ss ts cs
        "print_trains" -> printTrains ts
        "print_trains_v" -> printTrains' ts ss
        "print_route" -> printRoute ts cs (tail $ words input)
        "quit" -> quit (ts,ss,cs,rs)
        _ -> usage
    handleInput (ts,ss,cs,rs)

loadData :: IO ReservationData
loadData = do
    d <- readFile "data.txt"
    return (read d)

quit :: ReservationData -> IO()
quit resData = do
    writeFile "data.txt" (show resData)
    exitSuccess

printStations :: Stations -> Trains -> IO()
printStations ss ts
    | Map.null ss = putStrLn "No stations found"
    | otherwise   = putStrLn $ "Stations: "++stationNames
    where stationNames = intercalate ", " $ getStationNames (map (getStationByID ss) stationIDs)
          stationIDs   = map fst stations
          stations     = Map.toList ss

printStations' :: Stations -> Trains -> Crossings -> IO()
printStations' ss ts cs
    | Map.null ss = putStrLn "No stations found"
    | otherwise   = do
        putStrLn ""
        putStrLn $ init $ unlines (map (printStation' ss ts cs) (Map.toAscList ss))

printStation' :: Stations -> Trains -> Crossings -> (ID,Station) -> String
printStation' ss ts cs (_,NilStation) = "NilStation should not occur!\n"
printStation' ss ts cs (i,Station n)  = "Station #"++show i++" '"++n++"'"++crossing++"\n"++trainString++"\n"
	where crossing    = if elem i cs then " - Crossing" else ""
              trains      = getTrainsStoppingAt ts i
              trainString = show trains

printTrains :: Trains -> IO()
printTrains ts
    | Map.null ts = putStrLn "No trains found.."
    | otherwise   = putStrLn $ init $ unlines $ map(printTrain) (Map.toAscList ts)

printTrain :: (ID,Train) -> String
printTrain (i,Train n _ _ _) = "Train #" ++ (show i) ++ " '"++n++"'"

printTrains' :: Trains -> Stations -> IO()
printTrains' ts ss
    | Map.null ts = putStrLn "No trains found.."
    | otherwise   = do
        putStrLn ""
        putStrLn $ init $ unlines (map (printTrain' ss) (Map.toAscList ts))

printTrain' :: Stations -> (ID,Train) -> String
printTrain' ss (_,NilTrain) = "NilTrain should not occur!\n"
printTrain' ss (i,t)        = printTrain (i,t)++"\n"++cars++"\n"++route++"\n"++seats++"\n"
    where (Train n cs r minseats) = t
          cars                 = if null cs 
              then "\t No cars" 
              else "\tCars:"++(intercalate "," (map carInfo cs));
          route                = if null r
              then "\tNo stations" 
              else "\tRoute: "++ (intercalate "," $ getStationNames (map (getStationByID ss) r))
          seats                = "\tNon-reservable seats: "++(show minseats)
          carInfo (Car i _ ss) = " #"++show i++" ("++show ss++" seats)"
	
printRoute :: Trains -> Crossings -> [String] -> IO()
printRoute ts cs r = do
    if (length r) /= 2 
        then putStrLn "Correct syntax: print_route station1 station2"
        else print $ trainsServingRoute ts cs (fst route) (snd route)
    where route = printRoute' r

printRoute' :: [String] -> (From,To)
printRoute' route
    | null f    = (-1,-1)
    | null t    = (-1,-1)
    | otherwise = ((fst $ head f),(fst $ head t))
    where f = readDec $ head route
          t = readDec $ last route

getStationByID :: Stations -> StationID -> (StationID,Station)
getStationByID ss i
    | Map.member i ss = (i,ss Map.! i)
    | otherwise       = (0,NilStation)

getStationByID' :: Stations -> StationID -> Station
getStationByID' ss i = snd (getStationByID ss i)

getTrainByID :: Trains -> TrainID -> (TrainID,Train)
getTrainByID ts i
    | Map.member i ts = (i,ts Map.! i)
    | otherwise       = (0,NilTrain)

getTrainsStoppingAt :: Trains -> StationID -> [TrainID]
getTrainsStoppingAt ts s = Map.keys $ Map.filter(stops) ts
    where stops (Train _ _ ss _) = elem s ss

getStationNames :: [(StationID,Station)] -> [String]
getStationNames []                  = []
getStationNames ((_,NilStation):ss) = getStationNames ss
getStationNames ((i,Station n):ss)  = [n++" (#"++(show i)++")"] ++ getStationNames ss

getTrainNames :: [(TrainID,Train)] -> [String]
getTrainNames []                     = []
getTrainNames ((0,NilTrain):ts)      = getTrainNames ts
getTrainNames ((i,Train n _ _ _):ts) = [n++" (#"++(show i)++")"] ++ getTrainNames ts

trainsServingRoute :: Trains -> Crossings -> From -> To -> [[(From,To,TrainID)]]
trainsServingRoute ts cs f t 
    | routes == [[]] = routes
    | otherwise = delete [] $ routes
    where routes = nub $ directConnection ts f t ++ inDirectConnections ts cs f t

directConnection :: Trains -> From -> To -> [[(From,To,TrainID)]]
directConnection ts f t = [zip3 (repeat f) (repeat t) (Map.keys servingTrains)]
    where servingTrains = Map.filter (servesRoute f t) ts

inDirectConnections :: Trains -> Crossings -> From -> To -> [[(From,To,TrainID)]]
inDirectConnections ts cs f t 
    | f == t        = [[]]
    | null $ head initialRoutes = [[]]
    | otherwise          = concat $ map (inDirectConnections' ts cs t) initialRoutes
    where initialRoutes = concat $ map (directConnection ts f) cs

inDirectConnections' :: Trains -> Crossings -> StationID -> [(From,To,TrainID)] -> [[(From,To,TrainID)]]
inDirectConnections' ts cs finalStation routeSoFar
    | null routeSoFar = [[]]
    | trainTwice = [[]]
    | done           = [routeSoFar]
    | null $ head newRoutes = [[]]
    | otherwise      = concat $ map (inDirectConnections' ts cs finalStation) (map (routeSoFar ++) $ newRoutes)
    where
        done          = lastStop == finalStation
        lastStop      = (\(_,t,_) -> t ) $ last routeSoFar
        trainTwice    = (\x -> (length $ nub x) /= (length x)) $ map (\(_,_,tid) -> tid) routeSoFar
        possibleStops = (cs \\ ((map (\(f,_,_) -> f) routeSoFar)++[lastStop])) ++ [finalStation]
        newRoutes     =  concat $ map (directConnection ts lastStop) possibleStops

servesRoute :: From -> To -> Train -> Bool
servesRoute f t (Train _ _ ss _)
    | f == t = False
    | (notElem f ss) = False
    | (notElem t ss) = False
    | posF > posT = False
    | otherwise = True
    where 
        posF = last $ elemIndices f ss
        posT = last $ elemIndices t ss
