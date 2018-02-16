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
import Node2       as Node
import Heuristic


solve_astar :: State -> [Move]
solve_astar st = aStar (PairingHeap.fromList [initState st]) (Set.fromList []) succNodes heuristic isFinalNode

aStar:: (PairingHeap Node) -- Heap of Open nodes
        -> (Set Node) -- Set of seen nodes
        -> (Node -> (State->Int) -> [Node]) -- Function that returns successive nodes ,using heuristic
        -> (State -> Int) -- Heuristic that calculates cost of moving to that node
        -> (Node -> Bool) -- Function that returns true if desired node
        -> [Move] -- Result is a list of moves (In case of no solution returns error )

aStar open closed succNodes heuristic isFinalFn
                | (Heap.isEmpty open) = error "Unsolveable Puzzle " -- No Nodes to search
                | isFinalFn current = stepsTaken current
                | isntNew current = aStar open' closed succNodes heuristic isFinalFn -- If already seen discard
                | otherwise = aStar open'' closed' succNodes heuristic isFinalFn -- Expand current node , and place
                -- the result in the closed set
                where
                    current = Heap.findMin open -- Current node is cheapest in Heap
                    isntNew current = current `Set.member` closed
                    open' = Heap.deleteMin open
                    closed' = Set.insert current closed
                    neighbors = Node.expand current closed heuristic
                    open'' = visit neighbors open' -- Adds the neighbor nodes to the open set

main = putStrLn $ writeState (makeMoves state (solve_astar state ))
        where
            state = readState "aaabcd\neffbcd\ne.==cd\nggh...\n.ih.jj\n.ikkll\n"
