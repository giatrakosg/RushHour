module ReadState where
import GameState
import Data.Map as Map
import Data.List as List
import Data.Set as Set
-- in normal coordinates


getPairs::String->[(Key,Element)]
getPairs str = zip keys elems
                where
                    width = countWidth str
                    len   = countLength str
                    cleanStr = clean str
                    keys  = getKeys str -- returns keys used in Map
                    oris  = [findOri str x | x <- keys ] -- orientations used in Map
                    sizes = [getCarLen str x | x <- keys ]
                    stpos = [norm2cart len width (getCarStartNorm cleanStr x)  | x <- keys ]
                    elems = List.zip3 oris sizes stpos


readState::String->State
readState str = (State len wid ms ss)
                where
                    wid = countWidth str
                    len = countLength str
                    pairs = getPairs str
                    elems = List.map snd pairs
                    expnd = List.concat (List.map expand elems)
                    allNorm = List.map (cart2norm len wid ) expnd

                    ms = Map.fromList pairs
                    ss = Set.fromList allNorm
