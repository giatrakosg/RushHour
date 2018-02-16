module Heuristic
    ( heuristic
    ) where
import GameState
import Data.Set as Set
import Data.Map.Strict as Map
import Data.List as List
import ReadState
import WriteState

-- How far away from the end is the car
pos2end::State->Int
pos2end st@(State len wid ms _) = len - (snd lst)
                                where
                                    target = ms ! '='
                                    crt = trd3 target
                                    expnd = expand target
                                    lst = last expnd -- Furthest to the right

-- How many cars are in front of '='
carsfront::State->Int
carsfront st@(State len wid ms ss) = Set.size ss'
                                     where
                                         crt' =  ms ! '=' -- Position of '='
                                         crt = last $ expand crt'
                                         xC = fst crt
                                         yC = snd crt
                                         end = (xC,len)
                                         minR = cart2norm len wid crt
                                         maxR = cart2norm len wid end
                                         ss' = Set.intersection (Set.fromList [(minR + 1)..maxR]) ss




heuristic::State->Int
heuristic st = (carsfront st) + (pos2end st)
