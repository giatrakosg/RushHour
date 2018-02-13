-- Found at Chris Okasaki
-- Purely Functional Data Structures
-- page 210 (Edition 1998)


module PairingHeap
    ( PairingHeap
    ) where

import Heap

data PairingHeap a = E | T a [PairingHeap a] deriving (Show)

mergePairs [] = E
mergePairs [h] = h
mergePairs (h1:h2:hs) = merge (merge h1 h2) (mergePairs hs)

elemHeap::(Eq a)=>a->(PairingHeap a)->Bool
elemHeap x (E)     = False
elemHeap x (T y []) = x == y
elemHeap x (T y (h:hs)) = if x == y
    then True
    else or (h' : hs')
            where
                hs' = map (elemHeap x) hs
                h'  = elemHeap x h

instance Heap PairingHeap where
    empty = E
    isEmpty E = True
    isEmpty _ = False

    insert x h = merge (T x []) h

    merge h E = h
    merge E h = h
    merge h1@(T x hs1) h2@(T y hs2) =
        if x < y
            then T x (h2:hs1)
            else T y (h1:hs2)

    findMin E = error "empty heap"
    findMin (T x hs) = x

    deleteMin (T x hs) = mergePairs hs


fromList::(Ord a)=>[a]->PairingHeap a
fromList [] = E
fromList (x:xs) = insert x (fromList xs)
