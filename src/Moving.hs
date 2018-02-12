module Moving where
import WriteState
import GameState
import ReadState

import Data.Map as Map
import Data.List as List
import Data.Set as Set

neighbours::State->CarType->Orientation->[CartCoord]
neighbours st1@(State len wid ms) tp UpDir = [top',lst']
                                            where
                                                elm = ms ! tp
                                                ls = expand elm
                                                top = head ls
                                                lst = last ls
                                                top' = (addTup (top) (-1,0))
                                                lst' = (addTup (lst) (1,0))
neighbours st1@(State len wid ms) tp RightDir = [top',lst']
                                            where
                                                elm = ms ! tp
                                                ls = expand elm
                                                top = head ls
                                                lst = last ls
                                                top' = (addTup (top) (0,-1))
                                                lst' = (addTup (lst) (0,1))

inborder::State->CartCoord->Bool
inborder (State len wid ms) (x,y) = (0 < x && x <= wid) && (0 < y && y <= len)

isvalid::State->CartCoord->Bool
isvalid st1@(State len wid ms) (x,y) = (isempty st1 (cart2norm len wid (x,y))) && (inborder st1 (x,y))


-- Returns all possible moves of given CarType
carMoves::State->CarType->[Int]
carMoves st1@(State len wid ms) tp = List.map (cart2norm len wid) (List.filter (\x -> isvalid st1 x) nbr)
                                 where
                                     nbr = neighbours st1 tp (findOri (writeState st1) tp)

-- Checks if normal position c is empty in given State
isempty::State->Int->Bool
isempty (State len wid ms) x = not (x `Set.member` setPos)
                            where
                                elems = List.map snd (Map.toList ms)
                                posCart = List.map expand (elems)
                                posNorm = List.map (\x -> List.map (cart2norm len wid ) x ) posCart
                                flatPos = List.concat posNorm
                                setPos  = Set.fromList flatPos
