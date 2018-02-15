module Heuristic
    ( heuristic
    ) where
import GameState
import Data.Set as Set
import Data.Map as Map
import Data.List as List
import ReadState
import WriteState

-- How far away from the end is the car
pos2end::State->Int
pos2end st@(State len wid ms) = len - (snd lst)
                                where
                                    target = ms ! '='
                                    crt = trd3 target
                                    expnd = expand target
                                    lst = last expnd -- Furthest to the right

countinlineRight::CartCoord->[CartCoord]->Int
countinlineRight _ [] = 0
countinlineRight (a,b) (((x,y)):ls) = if (a == x) && (b < y)
    then 1 + (countinlineRight (a,b) ls)
    else 0 + (countinlineRight (a,b) ls)

-- How many cars are in front
carsfront::State->Int
carsfront st@(State len wid ms) = countinlineRight lst allCoords
                                    where
                                        elements = Map.elems ms
                                        elemCoords = List.map (\x -> expand x) elements
                                        allCoords = List.concat elemCoords
                                        target = ms ! '='
                                        crt = trd3 target
                                        expnd = expand target
                                        lst = last expnd -- Furthest to the right

heuristic::State->Int
heuristic st = (carsfront st) + (pos2end st)
