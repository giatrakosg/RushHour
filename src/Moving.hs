module Moving (successorMoves) where
import WriteState
import GameState
import ReadState

import Data.Map.Strict as Map
import Data.List as List
import Data.Set as Set

-- Returns square up / down of car type or left / right
neighbours::State->CarType->Orientation->[CartCoord]
neighbours (State _ _ ms _) tp UpDir = [top',lst']
                                            where
                                                elm = ms ! tp
                                                ls = expand elm
                                                top = head ls
                                                lst = last ls
                                                top' = (addTup (top) (-1,0))
                                                lst' = (addTup (lst) (1,0))
neighbours (State _ _ ms _) tp RightDir = [left,right]
                                            where
                                                elm = ms ! tp
                                                ls = expand elm
                                                top = head ls
                                                lst = last ls
                                                left = (addTup (top) (0,-1))
                                                right = (addTup (lst) (0,1))

inborder::State->CartCoord->Bool
inborder (State len wid _ _) (x,y) = ((0 < x) && (x <= wid)) && ((0 < y) && (y <= len))
isempty::State->Int->Bool
isempty (State len wid ms ss) x = not (x `Set.member` ss)
isvalid::State->CartCoord->Bool
isvalid st1@(State len wid _ _) (x,y) = (isempty st1 (cart2norm len wid (x,y))) && (inborder st1 (x,y))


-- Returns all valid move positions of given CarType
-- Finds the neighboring cells of tp and filters them by validity
carMoves::State->CarType->[CartCoord]
carMoves st1@(State len wid ms _) tp = List.filter (\x -> isvalid st1 x) nbr
                                 where
                                     nbr = neighbours st1 tp (fst3 $ ms ! tp )

turn2move::State->CarType->CartCoord->Move
turn2move st1@(State len wid ms _) tp crt
                                         | crtPos == crt = error "Invalid Move"
                                         | fst crtPos < fst crt = (tp,South)
                                         | fst crtPos > fst crt = (tp,North)
                                         | snd crtPos < snd crt = (tp,East)
                                         | snd crtPos > snd crt = (tp,West)
                                         where
                                             crtPos = trd3 $ ms ! tp

moveList::State->CarType->[CartCoord]->[Move]
moveList  _   _  [] = []
moveList st1 tp ls = List.map (\x -> (turn2move st1 tp x)) ls

deepMoveList::State->[CarType]->[[CartCoord]]->[[Move]]
deepMoveList _      _     [] = [[]]
deepMoveList _     []     _   = [[]]
deepMoveList st1 (t:tp) (l:ls) = (moveList st1 t l) : (deepMoveList st1 tp ls)

successorMoves::State->[(Move,Int)]
successorMoves st1@(State len wid ms _) = List.zip (List.concat $ deepMoveList st1 keys crtMvs) [1,1..]
                                        where
                                            -- list of keys
                                            keys = Map.keys ms
                                            -- list of valid moves for each car type
                                            crtMvs = List.map (\x -> carMoves st1 x) keys
