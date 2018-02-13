-- Found at Chris Okasaki
-- Purely Functional Data Structures
-- page 210 (Edition 1998)

-- There are extensions

module PairingHeap
    ( PairingHeap ,toList, fromList ,elemHeap
    ) where

import Heap

data PairingHeap a = E | T a [PairingHeap a] deriving (Show)

mergePairs [] = E
mergePairs [h] = h
mergePairs (h1:h2:hs) = merge (merge h1 h2) (mergePairs hs)

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
    findMin (T x _) = x

    deleteMin E        = E
    deleteMin (T _ hs) = mergePairs hs

-- EXTENSIONS TO OKASAKI IMPLEMENTATION
fromList::(Ord a)=>[a]->PairingHeap a
fromList [] = E
fromList (x:xs) = insert x (fromList xs)

elemHeap::(Eq a)=>a->(PairingHeap a)->Bool
elemHeap _ (E)     = False
elemHeap x (T y []) = x == y
elemHeap x (T y (h:hs)) = if x == y
    then True
    else or (h' : hs')
            where
                hs' = map (elemHeap x) hs
                h'  = elemHeap x h

toList::(PairingHeap a) -> [a]
toList E = []
toList (T x []) = [x]
toList (T x (h:hs)) = (x : hl) ++ rs'
                        where
                            rs = map toList hs -- Pass through list list of children
                            rs' = concat rs
                            hl = toList h
