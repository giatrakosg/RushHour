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
import Node
import Heuristic

initState :: State -> Node
initState st = Root st

solve_astar :: State -> [Move]
solve_astar st = aStar2 (PairingHeap.fromList [initState st]) (Set.fromList []) succNodes heuristic isFinalNode

{-
aStar :: (PairingHeap Node) -> (SeArmie Hammert Node) -> [State]
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
-}
aStar2:: (PairingHeap Node) -- Heap of Open nodes
        -> (Set Node) -- Set of seen nodes
        -> (Node -> (State->Int) -> [Node]) -- Function that returns successive nodes ,using heuristic
        -> (State -> Int) -- Heuristic that calculates cost of moving to that node
        -> (Node -> Bool) -- Function that returns true if desired node
        -> [Move] -- Result is a list of moves (In case of no solution returns error )

aStar2 open closed succNodes heuristic isFinalFn
                | (Heap.isEmpty open) = error "Unsolveable Puzzle " -- No Nodes to search
                | isFinalFn current = stepsTaken current
            --    | isntNew current = aStar2 open' closed succNodes heuristic isFinalFn -- If already seen discard
                | otherwise = aStar2 open'' closed' succNodes heuristic isFinalFn -- Expand current node , and place
                -- the result in the closed set
                where
                    current = Heap.findMin open -- Current node is cheapest in Heap
--                    isntNew current = current `Set.member` closed
                    open' = Heap.deleteMin open
                    closed' = Set.insert current closed
                    neighbors = Node.expand current closed heuristic
                    open'' = visit neighbors open' -- Adds the neighbor nodes to the open set
