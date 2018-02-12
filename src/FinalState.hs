module FinalState where
import GameState
import WriteState
import ReadState
import Data.Map as Map

finalState::State->Bool
finalState st1@(State len wid ms) = (xC,yC) `elem` expnd
                                    where
                                        keys = getKeys (writeState st1)
                                        keyElm = ms ! '='
                                        expnd = expand keyElm
                                        xC = fst $ expnd !! 0
                                        yC = len
