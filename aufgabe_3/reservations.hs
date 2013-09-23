module Main where

import System.Exit
import qualified Data.Map.Lazy as Map
import Data.List(intercalate,elemIndex,elemIndices,(\\),nub,delete,group,sortBy)
import Data.Maybe
import Data.Ord
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
type Station = String

data Car = Car CarID TrainID Seats  deriving (Show, Read)
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
        putStrLn "list_stations: Display list of stations"
        putStrLn "list_stations_v: Display list of stations including trains stopping there"
        putStrLn "list_trains: Display list of trains"
        putStrLn "list_trains_v: Display list of trains (verbose)"
        putStrLn "list_route from to: Display train connections between 'from' and 'to'"
        putStrLn "list_seat train car seat: Display list of reservations for seat #'seats' in car #'car' of train #'train'"
        putStrLn "list_reservation id: Display reservation data for reservation #'id'"
        putStrLn "max_reservations from to: List information about possible reservations between 'from' and 'to'"
        putStrLn "add_reservation from to route seats: Add reservation between 'from' and 'to', using route #'route', for a group of 'seats' passengers"
        putStrLn "del_reservation id: Delete reservation with given ID"
        putStrLn "quit: Quit program and save data"

-- Get line, choose function to call
-- Rinse, repeat
handleInput :: ReservationData -> IO()
handleInput old = do
    putStrLn ">> "
    input <- getLine
    case ( head (words input) ) of
        "help" -> usage
        "?" -> usage
        "list_stations" -> printStations ss
        "list_stations_v" -> printStations' ss ts cs
        "list_trains" -> printTrains ts
        "list_trains_v" -> printTrains' ts ss
        "list_route" -> printRoute ts ss cs (tail $ words input)
        "list_seat" -> printReservations old (tail $ words input)
        "list_reservation" -> printReservation old (tail $ words input)
        "max_reservations" -> printGroup old (tail $ words input)
        "add_reservation" -> printReservationPossible old (tail $ words input)
        "del_reservation" -> printDelReservationPossible old (tail $ words input)
        "d" -> d ts ss cs rs (tail $ words input)
        "quit" -> quit old
        _ -> usage
    let after = case (head (words input))  of
            "add_reservation" -> addReservation old (tail $ words input)
            "del_reservation" -> delReservation old (tail $ words input)
            _ -> old
    handleInput after
    where (ts,ss,cs,rs) = old

-- data.txt needs to contain a valid result of 'show ReservationData'
loadData :: IO ReservationData
loadData = do
    d <- readFile "data.txt"
    return (read d)

-- serialize and write out current data set
quit :: ReservationData -> IO()
quit resData = do
    writeFile "data.txt" (show resData)
    exitSuccess

-- call printStation for all available stations
printStations :: Stations -> IO()
printStations ss
    | Map.null ss = putStrLn "No stations found"
    | otherwise   = putStrLn $ intercalate ", " $ map (printStation) $ Map.toAscList ss

-- format station output
printStation :: (StationID,Station) -> String
printStation (sid,sname) = sname++" (#"++(show sid)++")"

-- call printStation' for all available stations
printStations' :: Stations -> Trains -> Crossings -> IO()
printStations' ss ts cs
    | Map.null ss = putStrLn "No stations found"
    | otherwise   = do
        putStrLn ""
        putStrLn $ init $ unlines (map (printStation' ss ts cs) (Map.toAscList ss))

-- form detailled station output
printStation' :: Stations -> Trains -> Crossings -> (ID,Station) -> String
printStation' ss ts cs (i,n)  = "Station #"++show i++" '"++n++"'"++crossing++"\n"++trainString
	where crossing    = if elem i cs then " - Crossing" else ""
              trains      = getTrainsStoppingAt ts i
              trainString = unlines $ map(printTrain.getTrainByID ts) trains

-- call printTrain for all available trains
printTrains :: Trains -> IO()
printTrains ts
    | Map.null ts = putStrLn "No trains found.."
    | otherwise   = putStrLn $ init $ unlines $ map(printTrain) (Map.toAscList ts)

-- format train output
printTrain :: (ID,Train) -> String
printTrain (i,Train n _ _ _) = "Train #" ++ (show i) ++ " '"++n++"'"

-- call printTrain' for all available trains
printTrains' :: Trains -> Stations -> IO()
printTrains' ts ss
    | Map.null ts = putStrLn "No trains found.."
    | otherwise   = do
        putStrLn ""
        putStrLn $ init $ unlines (map (printTrain' ss) (Map.toAscList ts))

-- format detailled train output
printTrain' :: Stations -> (ID,Train) -> String
printTrain' ss (_,NilTrain) = "NilTrain should not occur!\n"
printTrain' ss (i,Train n cs r min)        = printTrain (i,t)++"\n"++cars++"\n"++route++"\n"++seats++"\n"
    where cars    = if null cs
              then "\t No cars" 
              else "\tCars:"++(intercalate "," (map carInfo cs));
          route   = if null r
              then "\tNo stations" 
              else "\tRoute: "++ (intercalate "," $ map (printStation) $ map(getStationByID ss) r)
          seats   = "\tNon-reservable seats: "++(show min)
          carInfo = (\(Car i _ ss) -> " #"++show i++" ("++show ss++" seats)")
          t       = (Train n cs r min)

-- convert input, get possible routes, call printRoute' for all their parts
printRoute :: Trains -> Stations -> Crossings -> [String] -> IO()
printRoute ts ss cs r
    | length r /= 2 = putStrLn "Correct syntax: print_route station1 station2"
    | route == (-1,-1) = putStrLn "Invalid station IDs (check list_stations)"
    | routes == [[]] = putStrLn "No routes found"
    | otherwise = do
        putStrLn "Possible routes:"
        putStrLn $ init.unlines.concat$ map(addHeader) $ zip [1..] $ map(map(("\t"++).(printRoute' ts ss))) routes
    where route   = convertFromTo r
          routes  = getRoutes ts cs (fst route) (snd route)
          addHeader = (\(no,text)-> ["Route "++(show no)++":"]++text)          

-- print one segment of a route
printRoute' :: Trains -> Stations -> (From,To,TrainID) -> String
printRoute' ts ss (f,t,tid) = (printTrain (getTrainByID ts tid)) ++ " from: "++(printStation (getStationByID ss f))++" to: "++(printStation (getStationByID ss t))

-- convert input, print routes, printGroup' selected route
printGroup :: ReservationData -> [String] -> IO()
printGroup (ts,ss,cs,rs) input
    | length input /= 2 = putStrLn "Invalid number of arguments"
    | route == (-1,-1) = putStrLn "Invalid station IDs"
    | routes == [[]] = putStrLn "No routes found"
    | otherwise = do
        printRoute ts ss cs input
        putStrLn "Enter route number:"
        chosen <- getLine
        case (length.convertDec $ words chosen) of
            0 -> putStrLn "Invalid route selected"
            1 -> putStrLn $ printGroup' (ts,ss,cs,rs) routes (head.convertDec $ words chosen)
            _ -> putStrLn "Only one route can be selected"
    where route  = convertFromTo input
          routes = getRoutes ts cs (fst route) (snd route)

-- get total free seats, reservable free seats and max group size
printGroup' :: ReservationData -> [[(From,To,TrainID)]] -> Int -> String
printGroup' _ _ 0 = "Invalid route selected"
printGroup' (ts,ss,cs,rs) routes i = if i <= (length routes)
    then "Total: "++(show total)++"\nReservable: "++(show avl)++"\nBiggest group: "++(show group)
    else "Invalid route selected"
    where r     = routes !! (i-1)
          free  = map(getFreeSeatsSegment rs) $ map(\(f,t,tid)->(f,t,getTrainByID ts tid)) r
          total = map(getSeatSum) free
          min   = map((\(tid,Train _ _ _ min)->min).(\(_,_,tid)->getTrainByID ts tid)) r
          avl   = map (\(t,m)->t-m) $ zip total min
          group = map ((\(_,seats)->length seats).(\(m,f)->getBiggestGroup m f)) $ zip min free

printReservation :: ReservationData -> [String] -> IO()
printReservation (ts,ss,cs,rs) input
    | (length input) /= 1 = putStrLn "Invalid numer of arguments"
    | null conv = putStrLn "Invalid arguments"
    | all(null.snd) filtered = putStrLn "Invalid reservation ID"
    | otherwise = do
        putStrLn $ "Reservation #"++(show rid)
        if ((length output)==1)
            then putStrLn $ (head output)
            else putStrLn $ init.unlines $ output
    where conv     = convertDec input
          rid      = head conv
          output   = filter(""/=) $ map (printReservation' ss) $ map (\(tid,res) -> ((getTrainByID ts tid),res)) $ filtered
          filtered = filterRes rs rid

printReservation' :: Stations -> ((TrainID,Train),[Reservation]) -> String
printReservation' ss (t,res)
    | null res = ""
    | (length res) == 1 = "\t"++(printTrain t)++": "++resInfo ss (head res)
    | otherwise = "\t"++(printTrain t)++":\n"++(concat $ map(resInfo ss) res)
    where resInfo ss (Reservation rid cid seats f t) = "Car #"++(show cid)++", "++(show $ length seats)++" seats, from "++(printStation $ getStationByID ss f) ++ " to "++(printStation $ getStationByID ss t)

-- convert input and call printReservations'
printReservations :: ReservationData -> [String] -> IO()
printReservations res input
    | (length input) /= 3 = putStrLn "Invalid number of arguments"
    | null conv = putStrLn "Invalid arguments"
    | otherwise = putStrLn.init $ printReservations' res conv
    where conv = convertDec input

-- lookup and format reservation data for specified train/car/seat combination
printReservations' :: ReservationData -> [Int] -> String
printReservations' (ts,ss,cs,rs) [tid,cid,sid] 
    | Map.notMember tid rs = "No reservations for this train"
    | null res             = "No reservations for this seat"
    | otherwise            = unlines $ output res
    where res    = [(rid,f,t) | (Reservation rid rcid seats f t) <- (rs Map.! tid), rcid == cid, elem sid seats]
          name s = (getStationName ss s) ++ "(#"++(show s)++")"
          output = map(\(rid,f,t)->"Reservation #"++(show rid)++", from "++(name f)++" to "++(name t)) 

printReservationPossible :: ReservationData -> [String] -> IO()
printReservationPossible res input
    | possible  = putStrLn "Reservation added."
    | otherwise = putStrLn "This reservation is not possible."
    where possible = reservationPossible res input

printDelReservationPossible :: ReservationData -> [String] -> IO()
printDelReservationPossible (ts,ss,cs,rs) input
    | (length input) /= 1 = putStrLn "Wrong number of arguments"
    | null conv = putStrLn "Invalid arguments"
    | all(null.snd) res = putStrLn "Invalid reservation ID"
    | otherwise = putStrLn "Reservation deleted"
    where conv = convertDec input
          res  = filterRes rs (head conv)

addReservation :: ReservationData -> [String] -> ReservationData
addReservation res input
    | not possible = res
    | otherwise = addReservation' res input
    where possible = reservationPossible res input

addReservation' :: ReservationData -> [String] -> ReservationData
addReservation' res input = (ts,ss,cs,newrs (zip route toAdd))
    where [f,t,rid,seats] = convertDec input
          routes          = getRoutes ts cs f t
          route           = routes !! (rid-1)
          (ts,ss,cs,rs)   = res
          min             = map((\(tid,Train _ _ _ min)->min).(\(_,_,tid)->getTrainByID ts tid)) route
          free            = map (getFreeSeatsSegment rs.(\(f,t,tid)->(f,t,getTrainByID ts tid))) route
          toAdd           = map (\(cid,s)->(cid,take seats s)) $ map (\(m,f)->getBiggestGroup m f) $ zip min free
          newrs r         = foldl (addReservationFold) rs $ map (\((f,t,tid),(cid,seats))->(tid,(Reservation nextID cid seats f t))) r
          nextID          = (1+).maximum.concat $ map(map(\(Reservation id _ _ _ _)->id)) $ Map.elems rs

delReservation :: ReservationData -> [String] -> ReservationData
delReservation old input
    | (length input) /= 1 = old
    | null conv = old
    | otherwise = (ts,ss,cs,Map.map(filter(\(Reservation id _ _ _ _) -> id /= (head conv))) rs)
    where (ts,ss,cs,rs) = old
          conv = convertDec input

addReservationFold :: Reservations -> (TrainID,Reservation) -> Reservations
addReservationFold rs (tid,r) = Map.insertWith(++) tid  [r] rs

reservationPossible :: ReservationData -> [String] -> Bool
reservationPossible res input
    | (length input) /= 4 = False
    | null conv = False
    | otherwise = reservationPossible' res (f,t) r s
    where conv      = convertDec input
          [f,t,r,s] = conv

reservationPossible' :: ReservationData -> (From,To) -> Int -> Seats -> Bool
reservationPossible' res (f,t) ri s
    | routes == [[]] = False
    | (length routes) < ri = error $ "wrong route index " ++ (show ri) ++"/"++(show (length routes))
    | any(s>) group = False
    | otherwise = True
    where routes        = getRoutes ts cs f t
          route         = routes !! (ri-1)
          (ts,ss,cs,rs) = res
          min           = map((\(tid,Train _ _ _ min)->min).(\(_,_,tid)->getTrainByID ts tid)) route
          free          = map (getFreeSeatsSegment rs.(\(f,t,tid)->(f,t,getTrainByID ts tid))) route
          group         = map ((\(_,seats)->length seats).(\(m,f)->getBiggestGroup m f)) $ zip min free

filterRes :: Reservations -> ID -> [(TrainID,[Reservation])]
filterRes rs rid = map(\(tid,res) -> (tid,filter(\(Reservation id _ _ _ _)->id == rid) res)) $ Map.toAscList rs


-- convert input Strings to Int, returns [] if any input cannot be parsed
convertDec :: [String] -> [Int]
convertDec input
    | any (null) conv = []
    | otherwise = map (fst.head) conv
    where conv = map (readDec) input

-- convert two input strings into (From,To) tuple, (-1,-1) on parsing error
convertFromTo :: [String] -> (From,To)
convertFromTo route
    | null f    = (-1,-1)
    | null t    = (-1,-1)
    | otherwise = ((fst $ head f),(fst $ head t))
    where f = readDec $ head route
          t = readDec $ last route

-- lookup station by id, (-1,"") if non-existant
getStationByID :: Stations -> StationID -> (StationID,Station)
getStationByID ss i
    | Map.member i ss = (i,ss Map.! i)
    | otherwise       = (-1,"")

-- lookup station name, "" if non-existant
getStationName :: Stations -> StationID -> Station
getStationName ss i = snd (getStationByID ss i)

-- lookup train by id, (0,NilTrain) if non-existant
getTrainByID :: Trains -> TrainID -> (TrainID,Train)
getTrainByID ts i
    | Map.member i ts = (i,ts Map.! i)
    | otherwise       = (0,NilTrain)

-- lookup train name, "" if non-existant
getTrainName :: Trains -> TrainID -> String
getTrainName ts tid = name.snd$getTrainByID ts tid
    where name NilTrain         = ""
          name (Train n _ _ _ ) = n

-- get list of trains stopping at certain station
getTrainsStoppingAt :: Trains -> StationID -> [TrainID]
getTrainsStoppingAt ts s = Map.keys $ Map.filter(\(Train _ _ ss _) -> elem s ss) ts

-- filter reservations and call helper
getFreeSeatsSegment :: Reservations -> (From,To,(TrainID,Train)) -> [(CarID,[SeatID])]
getFreeSeatsSegment rs (f,t,(tid,Train _ cs ss _))
    | Map.notMember tid rs = map(getFreeSeatsSegment' [] f t) cs  -- no reservations for this train, all seats available
    | otherwise            = map(getFreeSeatsSegment' frs f t) cs -- reservations that interleave with (From,To) are blocking seats
    where frs = [r | r <- (rs Map.! tid), interleaving ss r f t]

-- filter reservations by car id, return list of non-reserved seats
getFreeSeatsSegment' :: [Reservation] -> From -> To -> Car -> (CarID,[SeatID])
getFreeSeatsSegment' rs f t (Car i _ seats) = (i,[1..seats] \\ resSeats)
    where resSeats = concat $ [ rseats | (Reservation _ cid rseats _ _ ) <- rs, cid == i] --reserved

-- get total number of free seats for list of available seats
-- probably used on result of getFreeSeatsSegment
getSeatSum :: [(CarID,[SeatID])] -> Seats
getSeatSum seats 
    | null seats = 0
    | otherwise = sum $ map (length.snd) seats

-- get biggest group of reservable seats
-- probably used in conjunction with getFreeSeatsSegment
getBiggestGroup :: MinSeats -> [(CarID,[SeatID])] -> (CarID,[SeatID])
getBiggestGroup min seats
    | available <= 0 = (0,[]) -- no more reservable seats
    | length best > available = (cid,take available best) -- limited by non-reservable seats
    | otherwise = (cid,best)
    where available  = (getSeatSum seats) - min -- free minus non-reservable seats
          possible   = map(getBiggestGroup' available) seats -- biggest group for every segment
          (cid,best) = head $ sortBy (comparing (((-1)*).length.snd)) $ possible -- filter best group

-- get biggest group for one segment
getBiggestGroup' :: Seats -> (CarID,[SeatID]) -> (CarID,[SeatID])
getBiggestGroup' maxAvl (cid,seats)
    | null seq = (0,[]) -- no available seats
    | otherwise = (cid,(head $ sortBy (comparing (((-1)*).length)) seq)) -- longest sequence
    where seq = sequences seats [[]] -- split seat list into list of consecutive sequences

-- get available routes between two stations
getRoutes :: Trains -> Crossings -> From -> To -> [[(From,To,TrainID)]]
getRoutes ts cs f t 
    | routes == [[]] = routes
    | otherwise = delete [] $ routes
    where routes = nub $ directConnections ts f t ++ inDirectConnections ts cs f t

-- get direct connections between two stations
directConnections :: Trains -> From -> To -> [[(From,To,TrainID)]]
directConnections ts f t = group $ zip3 (repeat f) (repeat t) (Map.keys servingTrains)
    where servingTrains = Map.filter (servesRoute f t) ts

-- get indirect connections between stations by searching for direct connections
-- between From, connected Crossings and To
inDirectConnections :: Trains -> Crossings -> From -> To -> [[(From,To,TrainID)]]
inDirectConnections ts cs f t 
    | f == t        = [[]]
    | null initialRoutes = [[]]
    | null $ head initialRoutes = [[]]
    | otherwise          = concat $ map (inDirectConnections' ts cs t) initialRoutes
    where initialRoutes = concat $ map (directConnections ts f) cs

-- recursive helper function
inDirectConnections' :: Trains -> Crossings -> StationID -> [(From,To,TrainID)] -> [[(From,To,TrainID)]]
inDirectConnections' ts cs finalStation routeSoFar
    | null routeSoFar       = [[]] -- invalid initial route
    | trainTwice            = [[]] -- loop, used same train twice
    | null newRoutes        = [[]] -- no more routes to check
    | null $ head newRoutes = [[]] -- ditto
    | done                  = [routeSoFar] -- reached finalStation
    | otherwise             = concat $ map (inDirectConnections' ts cs finalStation) (map (routeSoFar ++) $ newRoutes) -- recursion
    where
        done          = lastStop == finalStation
        lastStop      = (\(_,t,_) -> t ) $ last routeSoFar
        trainTwice    = (\x -> (length $ nub x) /= (length x)) $ map (\(_,_,tid) -> tid) routeSoFar
        possibleStops = (cs \\ ((map (\(f,_,_) -> f) routeSoFar)++[lastStop])) ++ [finalStation]
        newRoutes     =  concat $ map (directConnections ts lastStop) possibleStops

-- check whether given route is served by a train
servesRoute :: From -> To -> Train -> Bool
servesRoute f t (Train _ _ ss _)
    | f == t         = False -- that's no route ;)
    | (notElem f ss) = False -- From not part of train's route
    | (notElem t ss) = False -- To not part of train's route
    | posF > posT    = False -- Train goes the other way
    | otherwise      = True -- Train serves route
    where posF = last $ elemIndices f ss
          posT = last $ elemIndices t ss

-- check whether reservation and route are interleaving
interleaving :: [StationID] -> Reservation -> From -> To -> Bool
interleaving ss (Reservation _ _ _ fr tr) f t
    | any (isNothing) ind = False -- error
    | ti <= fri = False -- to is before reservation
    | tri <= fi = False -- reservation is before from
    | otherwise = True -- interleaving
    where ind = map (flip elemIndex ss) [fr,tr,f,t]
          fri = fromJust $ ind!!0
          tri = fromJust $ ind!!1
          fi  = fromJust $ ind!!2
          ti  = fromJust $ ind!!3

-- recursively breaks (sorted) list of ints into list of consecutive sequences
-- i.e. [1,2,3,6,7,9,10] -> [[1,2,3],[6,7],[9,10]]
sequences :: [Int] -> [[Int]] -> [[Int]]
sequences [] result     = result
sequences (i:is) [[]]   = sequences is [[i]]
sequences (i:is) (r:rs)
    | i - lastI == 1    = sequences is ([r++[i]]++rs)
    | otherwise         = sequences is ([[i]]++[r]++rs)
    where lastI = last r

-- debugging method
d :: Trains -> Stations -> Crossings -> Reservations -> [String] -> IO()
d ts ss cs rs r = do
    putStrLn "Free seats"
    putStrLn $ show seats
    putStrLn $ "Free seats: "++ (show $ sum $ map (length.snd) seats)
    putStrLn $ "Biggest group: "++(show group)
    where route = convertFromTo r
          seats = getFreeSeatsSegment rs (fst route,snd route,getTrainByID ts 1)
          group = getBiggestGroup 10 seats


