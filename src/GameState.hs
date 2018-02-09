
import Data.Char
import Data.Map as Map
import Data.List as List
import Data.Set as Set

-- Representation of State
-- M N (Map CarType (Orientation,Size,Pos))
-- The cars are represented by their size
-- their starting position and the direction the point to
-- The horizontal oriented cars start position is on the
-- left , the vertical oriented cars start position is on the
-- bottom

-- Board Coordinates Representation
-- i\j 1 2  3   4  width = 3 , length = 4
--  1  1 2   3  4 normal coordinates [1..12]
--  2  5 6   7  8 Cartesian (i,j)
--  3  9 10 11 12


data Direction = North | South | East | West deriving (Show,Eq)
-- A move is a list of CarType and Directions that the Car
-- is moved towards
type Move = (CarType,Direction)
getDir::Move->Direction
getDir (_,x) = x
getType::Move->CarType
getType (x,_) = x



type CarSize = Int
-- Color of the car
type CarType = Char
-- Cartesian Coordinates of the Car
type CartCoord = (Int,Int)

data Orientation = UpDir | RightDir deriving (Show)

-- We store as values of Map  the size and the orientation
type Element = (Orientation,CarSize,CartCoord)

type Key = CarType

data State = State Int Int (Map Key Element)
                deriving (Show)


countLength::String->Int
countLength ('\n':rs) = 0
countLength (c:rs) = 1 + (countLength rs)

countWidth::String->Int
countWidth str = length (List.filter (\x-> x == '\n') str)

-- Returns the unique Characters of the String , excluding '.' and '\n'
-- It turns the string to a Set thus making all unique but keeping '.' ,'\n'
-- Then it applies filter function
getKeys::String->[Key]
getKeys str = List.filter (\x-> x /= '\n' && x /= '.') $ Set.toList $ Set.fromList str

getCarLen::String->CarType->CarSize
getCarLen str tp = length $ List.filter (\x -> x == tp) str

-- in normal coordinates
getCarStartNorm::String->Char->Int
getCarStartNorm (c:str) a = if a == c
    then 0
    else 1 + (getCarStartNorm str a )

norm2cart::Int->Int->Int->CartCoord
norm2cart rc len width = (rc `div` width , rc `mod` len)

cart2norm::CartCoord->Int->Int->Int
cart2norm (x,y) len wid = (x-1)*len + y ;

-- Replace Char b in string with char c
replaceStr::String->Char->Char->String
replaceStr [] _ _ = []
replaceStr (a:str) c b = if (a == b) then c : (replaceStr str c b)
    else a:(replaceStr str c b)

-- Give a String Create List of String split at \n
listify::String->[String]
listify str = words str'
                where str' = replaceStr str ' ' '\n'


-- Finds orientation of CarType in String
-- If the Car is horizontal that it will have all its elements in one line
-- But if it is vertical in more that one
-- We split the string into width lists at '\n'
-- and count how many times the ctype is found in them
-- using list comprehension
findOri::String->CarType->Orientation
findOri str ctype = if length matches == 1
                    then RightDir else UpDir
                        where
                            str' = listify str
                            matches = [x | x <- str' , ctype `List.elem` x ]

-- Returns List of Keys and Elements used in internal State Map
-- Keys = ctype
-- Element = (Orientation,CarSize,StartPos)

getPairs::String->[(Key,Element)]
getPairs str = zip keys elems
                where
                    width = countWidth str
                    len   = countLength str
                    keys  = getKeys str
                    oris  = [findOri str x | x <- keys ]
                    sizes = [getCarLen str x | x <- keys ]
                    stpos = [norm2cart (getCarStartNorm str x) len width | x <- keys ]
                    elems = List.zip3 oris sizes stpos


{-
readState::String->State
readState str = (State wid len ms)
                where
                    wid = countWidth str
                    len = countLength len
-}


-- gets the Direction of the element
getOr::Element->Orientation
getOr (x,_,_) = x

--gets type of element
--getType::Element->CarType
--getType (_,_,x) = x


-- gets Size of element
getSize::Element->CarSize
getSize(_,x,_) = x

getCoord::Element->CartCoord
getCoord(_,_,x) = x

newVal::Element->Direction->Element
newVal (c,d,(x,y)) a
    | a == North = (c,d,(x+1,y))
    | a == South = (c,d,(x-1,y))
    | a == East  = (c,d,(x,y+1))
    | a == West  = (c,d,(x,y-1))

m = (State 2 3 (Map.fromList [('c',(UpDir,3,(1,1))),('a',(RightDir,6,(2,2)))]))

makeMove::State->Move->State
makeMove (State h w ms) (tp,dir) = (State h w ms')
                            where
                                oldVal =  ms ! tp
                                val' = newVal oldVal dir
                                ms' = Map.insert tp val' ms
