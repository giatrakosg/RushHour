module Moving (successorMoves) where
import WriteState
import GameState
import ReadState

import Data.Map.Strict as Map
import Data.List as List
import Data.Set as Set

getVertNorms::Int->Int->Int->[Int]
getVertNorms i len wid = [i + (j*len) | j <- [0..(wid - 1)]]

-- Returns all squares up / down of car type or left / right
neighbours::State->CarType->Orientation->[CartCoord]
neighbours (State len wid ms ss) tp UpDir = up ++ down
                                            where
                                                elm = ms ! tp -- The element to move
                                                (x0,y0) = trd3 elm -- Coordinates of element
                                                sz = snd3 elm -- Size of element
                                                up = [(x,y0) | x <- [1..(x0-1)]] -- All positions above
                                                down = [(x,y0)| x <- [(x0+1)..(wid-sz+1)]] -- All positions below it can fit
neighbours (State len wid ms ss) tp RightDir = left ++ right
                                            where
                                                elm = ms ! tp
                                                (x0,y0) = trd3 elm
                                                sz = snd3 elm
                                                left = [(x0,y) | y <- [1..(y0-1)]]
                                                right = [(x0,y) | y <- [(y0+1)..(len-sz+1)]]

inborder::State->CartCoord->Bool
inborder (State len wid _ _) (x,y) = ((0 < x) && (x <= wid)) && ((0 < y) && (y <= len))
isempty::State->CartCoord->CarType->Bool
isempty (State len wid ms ss) (x,y) tp = not (crt `Set.member` ss')
                                  where
                                      elm = ms ! tp
                                      expnSet = Set.fromList $ List.map (cart2norm len wid) $ expand elm
                                      ss' = Set.difference ss expnSet
                                      crt = cart2norm len wid (x,y)

nocollision::State->CartCoord->CarType->Bool
nocollision st@(State len wid ms ss) to tp = if (Set.size ss'' == 0)
    then True
    else False
                                             where
                                                 elm = ms ! tp
                                                 ori = fst3 elm
                                                 sz = snd3 elm
                                                 newEl = (ori,sz,to)
                                                 exp' = expand newEl
                                                 expSet' = Set.fromList $ List.map (cart2norm len wid ) exp'
                                                 expn = Set.fromList $ List.map (cart2norm len wid) $ expand elm -- previous positions
                                                 ss' = Set.difference ss expn
                                                 ss'' = Set.intersection ss' expSet'

isvalid::State->CartCoord->CarType->Bool
isvalid st1@(State len wid _ _) (x,y) tp = (isempty st1 (x,y) tp) && (inborder st1 (x,y))
                                            && (nocollision st1 (x,y) tp)


-- Returns all valid move positions of given CarType
-- Finds the neighboring cells of tp and filters them by validity
carMoves::State->CarType->[CartCoord]
carMoves st1@(State len wid ms _) tp = List.filter (\x -> isvalid st1 x tp) nbr
                                 where
                                     nbr = neighbours st1 tp (fst3 $ ms ! tp )

turn2move::State->CarType->CartCoord->Move
turn2move st1@(State len wid ms _) tp crt
                                         | oldPos == crt = error "Invalid Move"
                                         | fst oldPos < fst crt = (tp,South,(fst crt) - (fst oldPos))
                                         | fst oldPos > fst crt = (tp,North,(fst oldPos) - (fst crt))
                                         | snd oldPos < snd crt = (tp,East ,(snd crt) - (snd oldPos))
                                         | snd oldPos > snd crt = (tp,West ,(snd oldPos) - (snd crt))
                                         where
                                             oldPos = trd3 $ ms ! tp -- Old position

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
