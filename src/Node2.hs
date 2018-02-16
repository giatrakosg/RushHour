module Node2 where

import GameState as GameState
import Data.List as List
import Moving    as Moving
import ReadState
import Heap     as Heap
import PairingHeap as PairingHeap
import FinalState as FinalState
import Data.Set    as Set

data Node = Root
            | Node { state :: State ,
                     moves :: [Move] ,
                     gScore :: Int ,
                     hScore :: Int ,
                     prev :: Node }
                    deriving (Show)

instance Eq Node where
    m == n = (state m ) == (state n)
instance Ord Node where
    n       <= Root = False
    Root <= n = True
    m <= n = ((gScore m) + (hScore m) ) <= ((gScore n) + (hScore n))

initState :: State -> Node
initState st = (Node st [] 0 0 Root)


makeMoves::State->[Move]->State
makeMoves st ls = List.foldl' GameState.makeMove st ls

-- Given a node and the empty list , returns the state
getState::Node->[Move]->State
getState nd _ = state nd


-- Function that given a node and a heuristic returns list of nodes
succNodes::Node->(State->Int)->[Node]
succNodes (Root) heuristic = []
succNodes nd heuristic = List.map (\(x,y) -> Node (makeMove state x) ((moves nd) ++ [x]) (y + (gScore nd)) (heuristic (makeMove state x)) nd) (successorMoves state)
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
    else Node2.find x ys

-- Returns list of Moves from Root State
stepsTaken::Node->[Move]
stepsTaken (Root ) = []
stepsTaken m    = moves m

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
        Just dupN         = Node2.find n open'    -- Node with same state
        open''            = Heap.insert n open

-- Expand all neighbor nodes to node that are not already in closed
-- By using a Set we ensure that we don't have duplicates
expand :: Node -> Set Node -> (State -> Int) ->[Node]
expand node closed heuristic = Set.toList (Set.difference nodeSet closed )
                    where
                        nodes = succNodes node heuristic
                        nodeSet    = Set.fromList nodes
