import GameState

readState::String->State
readState str = (State wid len ms)
                where
                    wid = countWidth str
                    len = countLength str
                    pairs = getPairs str
                    ms = Map.fromList pairs
