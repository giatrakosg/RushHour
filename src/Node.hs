module Node where

import GameState as GameState
import Data.List as List
import Moving    as Moving
import ReadState
import Heap     as Heap
import PairingHeap as PairingHeap
import FinalState as FinalState
import Data.Set    as Set

data Node = Root (GameState.State )
            | Node { move :: Move ,
                     gScore :: Int ,
                     hScore :: Int ,
                     prev :: Node }
                    deriving (Show)

instance Eq Node where
    m == n = (getState m []) == (getState n [])
instance Ord Node where
    n       <= (Root m) = False
    (Root m) <= n = True
    m <= n = ((gScore m) + (hScore m) ) <= ((gScore n) + (hScore n))

makeMoves::State->[Move]->State
makeMoves st ls = List.foldl' GameState.makeMove st ls

-- Given a node and the empty list , returns the state
getState::Node->[Move]->State
getState (Root st) mvs = List.foldl' GameState.makeMove st mvs
getState (nd) mvs = getState (prev nd) (mvs ++ [move nd])

-- Function that given a node and a heuristic returns list of nodes
succNodes::Node->(State->Int)->[Node]
succNodes (Root st) heuristic = List.map (\(x,y) -> Node x y h (Root st)) (successorMoves st)
                                where
                                    h = heuristic st
succNodes nd heuristic = List.map (\(x,y) -> Node x (y + (gScore nd)) (heuristic (makeMoves state (ls' ++ [x]) )) nd) (successorMoves state)
                            where
                                state = getState nd []
                                ls' = stepsTaken nd
-- Replaces node in list with other node
-- Used two nodes have the same state but different costs
replace :: Node -> [Node] -> [Node]
replace _    []   = []
replace new (x:xs)
    | new == x = new : xs
    | otherwise      = x   : (replace new xs)

find::Node->[Node]->Maybe Node
find _ [] = Nothing
find x (y:ys) = if x == y
    then Just y
    else Node.find x ys

-- Returns list of Moves from Root State
stepsTaken::Node->[Move]
stepsTaken (Root _) = []
stepsTaken m    = (stepsTaken (prev m)) ++ [move m]

isFinalNode::Node->Bool
isFinalNode nd = finalState (getState nd [])

visit :: [Node] -> (PairingHeap Node) -> (PairingHeap Node)
visit [] open = open
visit (n:ns) open
    | isNewNeighbor     = visit ns (Heap.insert n open) -- If it is new add it to the list of open
    | isCheaperNeighbor = visit ns open'' -- If it is cheaper replace it
    | otherwise         = visit ns open -- If it not new , or cheaper to get to the State then continue
    where
        isNewNeighbor     = not (n `elem` open') -- State doesn't exist
        isCheaperNeighbor = ((gScore n) + (hScore n)) < ((gScore dupN) + (hScore dupN))  -- there is a way to go to the same
                                                    -- state cheaper
        open'             = PairingHeap.toList open
        Just dupN         = Node.find n open'    -- Node with same state
        open''            = Heap.insert n open

-- Expand all neighbor nodes to node that are not already in closed
-- By using a Set we ensure that we don't have duplicates
expand :: Node -> Set Node -> (State -> Int) ->[Node]
expand node closed heuristic = Set.toList (Set.difference nodeSet closed )
                    where
                        nodes = succNodes node heuristic
                        nodeSet    = Set.fromList nodes
