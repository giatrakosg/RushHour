module Main where

import Heap        as Heap
import PairingHeap as PairingHeap
import Data.Set    as Set
import Data.List   as List
import Moving      as Moving
import GameState   as GameState
import ReadState
import WriteState
import FinalState
data Node = Root
            | Node { state :: State ,
                     gScore :: Int ,
                     hScore :: Int ,
                     fScore :: Int ,
                     prev :: Node }
                    deriving (Show)

instance Eq Node where
    m == n = state m == state n
instance Ord Node where
    m <= n = (fScore m <= fScore n)

-- Expand all neighbor nodes to node that are not already in closed
-- By using a Set we ensure that we don't have duplicates

expand :: Node -> Set Node -> [Node]
expand node closed = Set.toList (Set.union closed nodeSet)
                    where
                        succMoves = List.map fst (successorMoves (state node))
                        succStates = List.map (makeMove (state node) ) succMoves
                        succNodes = List.map (\x ->Node x gScore' hScore' fScore' node ) succStates
                        gScore' = (gScore node) + 1
                        hScore' = 0
                        fScore' = gScore' + hScore'
                        nodeSet    = Set.fromList succNodes

find::Node->[Node]->Maybe Node
find _ [] = Nothing
find x (y:ys) = if x == y
    then Just y
    else Main.find x ys

visit :: [Node] -> (PairingHeap Node) -> (PairingHeap Node)
visit [] open = open
visit (n:ns) open
    | isNewNeighbor     = visit ns (Heap.insert n open) -- If it is new add it to the list of open
    | isCheaperNeighbor = visit ns (PairingHeap.fromList (replace n open')) -- If it is cheaper replace it
    | otherwise         = visit ns open -- If it not new , or cheaper to get to the State then continue
    where
        isNewNeighbor     = not (n `elem` open') -- State doesn't exist
        isCheaperNeighbor = fScore n < fScore dupN  -- there is a way to go to the same
                                                    -- state cheaper
        open'             = PairingHeap.toList open
        Just dupN         = Main.find n open'    -- Node with same state

replace :: Node -> [Node] -> [Node]
replace _    []   = []
replace new (x:xs)
    | new == x = new : xs
    | otherwise      = x   : replace new xs


stepsTaken::Node->[State]
stepsTaken Root = []
stepsTaken m    = (state m) : (stepsTaken (prev m))

initState :: State -> Node
initState st = Node st gS hS fS Root
                    where
                        gS = 0
                        hS = 0
                        fS = gS + hS
solve_astar :: State -> [State]
solve_astar st = aStar (PairingHeap.fromList [fnod]) (Set.fromList [])
                        where
                            fnod = initState st

aStar :: (PairingHeap Node) -> (Set Node) -> [State]
aStar open closed
        | (Heap.isEmpty open) = error "Unsolveable puzzle."
        | isntNew   = aStar open' closed
        | isGoal    = stepsTaken current
        | otherwise = aStar open'' closed'
        where
            current   = Heap.findMin open
            isntNew   = current `Set.member` closed
            isGoal    = finalState (state current)
            open'     = Heap.deleteMin open
            neighbors = (Main.expand) current closed
            open''    = visit neighbors open'
            closed'   = Set.insert current closed

main = putStrLn (List.concat (List.map writeState (solve_astar (readState "...\n==a\n..a"))))
