
import Data.Char
--import Move
--import Data.List
import Data.Map as Map
import Data.List as List
import Data.Set as Set
-- Representation of State
-- M N Map (StartPos,CarInfo)
-- The cars are represented by their size
-- their starting position and the direction the point to
-- The horizontal oriented cars start position is on the
-- left , the vertical oriented cars start position is on the
-- bottom
-- eg.
--j0123i
-- ....0  d = (i,j) = (1,0)
-- .ddd1  a = (i,j) = (3,0)
-- a...2
-- a...3

data Direction = North | South | East | West deriving (Show,Eq)

-- A move is a list of CarType and Directions that the Car
-- is moved towards

type Move = (CarType,Direction)
getDir (_,x) = x
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
getKeys::String->[Key]
getKeys str = List.filter (\x-> x /= '\n' && x /= '.') $ Set.toList $ Set.fromList str

getCarLen::CarType->String->CarSize
getCarLen tp str  = length $ List.filter (\x -> x == tp) str

-- in normal coordinates
getCarStartNorm::String->Char->Int
getCarStartNorm (c:str) a = if a == c
    then 0
    else 1 + (getCarStartNorm str a )

norm2cart::Int->Int->Int->CartCoord
norm2cart rc len width = (rc `div` width , rc `mod` len)

cart2norm::CartCoord->Int->Int->Int
cart2norm (x,y) len wid = (x-1)*len + y ;

{-
findOri::String->CarType->Orientation
findOri str tp = if condition then expression else expression
                    where
                        fstApr = norm2cart $ getCarStartNorm str tp
-}
--getPairs::String->[(Key,Element)]
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
