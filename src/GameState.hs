module GameState  where

import Data.Map as Map
import Data.List as List
import Data.Set as Set


-- Direction a car move towards
data Direction = North | South | East | West deriving (Show,Eq)

-- A move is a list of CarType and Directions that the Car
-- is moved towards
type Move = (CarType,Direction)

getDir::Move->Direction
getDir (_,x) = x
getType::Move->CarType
getType (x,_) = x


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

type CarSize = Int
-- Color of the car
type CarType = Char
-- Cartesian Coordinates of the Car
type CartCoord = (Int,Int)

data Orientation = UpDir | RightDir deriving (Show,Eq)

-- We store as values of Map  the size and the orientation
type Element = (Orientation,CarSize,CartCoord)

type Key = CarType

-- State Length Width (Map Positions)
data State = State Int Int (Map Key Element)
                deriving (Show,Eq)


countLength::String->Int
countLength []       = 0
countLength ('\n':_) = 0
countLength (_:rs)   = 1 + (countLength rs)

countWidth::String->Int
countWidth str = length $ listify str

-- Returns the unique Characters of the String , excluding '.' and '\n'
-- It turns the string to a Set thus making all unique but keeping '.' ,'\n'
-- Then it applies filter function
getKeys::String->[Key]
getKeys str = List.filter (\x-> x /= '\n' && x /= '.') $ Set.toList $ Set.fromList str

-- Gets Length of Car by counting how many occurences there are
getCarLen::String->CarType->CarSize
getCarLen str tp = length $ List.filter (\x -> x == tp) str

norm2cart::Int->Int->Int->CartCoord
norm2cart len wid rc = head [(x,y) | x <- [1..wid] , y<- [1..len] , cart2norm len wid (x,y) == rc]
cart2norm::Int->Int->CartCoord->Int
cart2norm len _ (x,y) = (x-1)*len + y ;

-- Replace Char b in string with char c
replaceStr::String->Char->Char->String
replaceStr [] _ _ = []
replaceStr (a:str) c b = if (a == b) then c : (replaceStr str c b)
    else a:(replaceStr str c b)

-- Give a String Create List of String split at \n
listify::String->[String]
listify str = words str'
                where str' = replaceStr str ' ' '\n'

-- Returns string without the '\n'
clean::String->String
clean str = List.foldr (++) [] str'
                where
                    str' = listify str

-- Finds orientation of CarType in String
-- If the Car is horizontal then it will have all its elements in one line
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

-- Generic functions used for handling 3-tuple
fst3::(a,b,c)->a
fst3 (x,_,_) = x
snd3::(a,b,c)->b
snd3 (_,x,_) = x
trd3::(a,b,c)->c
trd3 (_,_,x) = x

addTup::(Int,Int)->(Int,Int)->(Int,Int)
addTup (x1,y1) (x2,y2) = (x1 + x2 , y1 + y2)

-- Applies f with argument 1 from left list and argument 2 from right list
-- returning results in list
twofold::(a->b->c)->[a]->[b]->[c]
twofold _ _ [] = []
twofold _ [] _ = []
twofold f (x:xs) (y:ys) = (f x y) : (twofold f xs ys)

-- Returns list of cartesian coordinates occupied by the Element
-- The start position of RightDir elements is the leftmost square
-- The start position of UpDir    elements is the upmost square
expand::Element->[CartCoord]
expand (RightDir,size,(x,y)) = [(x,y + l) | l <- [0..(size -1)]]
expand (UpDir,size,(x,y)) = [(x + l,y) | l <- [0..(size -1)]]

tuplify::a->[b]->[(a,b)]
tuplify tp ls = zip (repeat tp) ls

deeptuples::[a]->[[b]]->[[(a,b)]]
deeptuples x y = twofold tuplify x y

getOr::Element->Orientation
getOr x = fst3 x
getSize::Element->CarSize
getSize x = snd3 x
getCoord::Element->CartCoord
getCoord x = trd3 x

-- The start position in Element is always the
-- top most for UpDir and Left Most for RightDir
newVal::Element->Direction->Element
newVal (c,d,(x,y)) a
    | a == North = (c,d,(x-1,y))
    | a == South = (c,d,(x+1,y))
    | a == East  = (c,d,(x,y+1))
    | a == West  = (c,d,(x,y-1))

makeMove::State->Move->State
makeMove (State h w ms) (tp,dir) = (State h w ms')
                            where
                                oldVal =  ms ! tp
                                val' = newVal oldVal dir
                                ms' = Map.insert tp val' ms
index::String->Char->Int
index [] _ = 1
index (a:str) c = if a == c
    then 1
    else 1 + (index str c)

getCarStartNorm::String->Char->Int
getCarStartNorm str c = index (clean str) c
