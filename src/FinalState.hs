module FinalState where
import GameState
import ReadState
import Data.Map as Map

finalState::State->Bool
finalState (State len _ ms _) = (xC,yC) `elem` expnd
                                    where
                                        keyElm = ms ! '='
                                        expnd = expand keyElm
                                        xC = fst (expnd !! 0)
                                        yC = len
