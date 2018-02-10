module ReadState
import GameState
import Data.Map as Map
import Data.List as List

-- in normal coordinates
getCarStartNorm::String->Char->Int
getCarStartNorm (c:str) a = if a == c
    then 1
    else 1 + (getCarStartNorm str a )


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
readState str = (State len wid ms)
                where
                    wid = countWidth str
                    len = countLength str
                    pairs = getPairs str
                    ms = Map.fromList pairs
