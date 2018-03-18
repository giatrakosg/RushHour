module Main where

import AStar
import ReadState
import WriteState
import Node 

main = putStrLn $ writeState (makeMoves state (solve_astar state ))
        where
            state = readState "aaabcd\neffbcd\ne.==cd\nggh...\n.ih.jj\n.ikkll\n"
