
import GameState
import Data.List as List
import Data.Map as Map
import Data.Ord as Ord

writeState::State->String
writeState (State len width ms) = List.map fst srtPos
                                    where
                                        ls = Map.toList ms -- list of (keys,element)
                                        elems = List.map snd ls -- list of elements
                                        keys = List.map fst ls
                                        expElems = List.map expand elems -- list of expanded elems
                                        normExpElems = List.map ((\x -> List.map (cart2norm len width ) x )) expElems -- Normalized
                                        flatExp = List.concat normExpElems
                                        -- Normal coordinates of empty cells
                                        -- List of all norm position not occupied
                                        dotPos = [x | x <- [1..(width * len)] , not (x `elem` flatExp)]
                                        -- Turn to tuple ('.',Position)
                                        dots = tuplify '.' dotPos
                                        expndElems = deeptuples keys normExpElems
                                        allPos = expndElems ++ [dots]
                                        flatPos = List.concat allPos
                                        srtPos = List.sortBy (comparing snd) flatPos
