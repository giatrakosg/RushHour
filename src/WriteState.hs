module WriteState where

import GameState
import ReadState
import Data.List as List
import Data.Map.Strict as Map
import Data.Ord as Ord

splitTillEmpty::[a]->Int->[[a]]
splitTillEmpty [] _ = [[]]
splitTillEmpty ys n = lft : (splitTillEmpty rgt n)
                        where
                            tup = List.splitAt n ys
                            lft = fst tup
                            rgt = snd tup

-- Adds the char c every x'th char of String
addCharEvery::String->Char->Int->String
addCharEvery str c x = List.concat withBreak
                        where
                            splat = splitTillEmpty str x
                            withBreak = List.map (\x -> x ++ [c]) splat
-- Write State works as follows :
-- 1. Find list of expanded elements (see expand)
-- 2. Convert to normal coordinates
-- 3. Find all positions not occupied by the expanded elements
-- 4. Make a list ('.',x) where x member of set of unused positions
-- 5. Append the lists and sort them by normal coordinates
-- 6. Add 'n' at every length element
-- 7. Print one after the other

writeState::State->String
writeState (State len width ms _) = init $ init $ addCharEvery (List.map fst srtPos) '\n' len
                                    where
                                        ls = Map.toList ms -- list of (keys,element)
                                        elems = List.map snd ls -- list of elements
                                        keys = List.map fst ls -- list of keys
                                        expElems = List.map expand elems -- list of expanded elems
                                        normExpElems = List.map ((\x -> List.map (cart2norm len width ) x )) expElems -- Normalized
                                        flatExp = List.concat normExpElems
                                        -- Normal coordinates of empty cells
                                        -- List of all norm position not occupied
                                        dotPos = [x | x <- [1..(width * len)] , not (x `elem` flatExp)]
                                        -- Turn to tuple ('.',Position)
                                        dots = tuplify '.' dotPos
                                        -- Turn to [[('a',Pos1),..],[('b',Pos1)..]]
                                        expndElems = deeptuples keys normExpElems
                                        allPos = expndElems ++ [dots]
                                        flatPos = List.concat allPos
                                        srtPos = List.sortBy (comparing snd) flatPos
