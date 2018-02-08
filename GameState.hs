import Data.Char
--import Data.List
import Data.Map as Map

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

type CarType = Char
type CartCoord = (Int,Int)
data Orientation = UpDir | RightDir deriving (Show)

type Element = (CarType,Orientation,CartCoord)
data State = State Int Int (Map CartCoord Element)
                deriving (Show)

y::(CartCoord,Element)
y = ((1,2),('c',UpDir,(1,2)))
x::(CartCoord,Element)
x = ((2,1),('c',UpDir,(2,1)))

-- gets the cartesian coordinates of the element
getCoordinates::Element->CartCoord
getCoordinates (_,_,x) = x

-- gets the Direction of the element
getDir::Element->Orientation
getDir (_,x,_) = x

--gets type of element
getType::Element->CarType
getType (x,_,_) = x
-- Adds element to State
addElement::State->Element->State
addElement (State n l m) k = (State n l m')
                                where
                                    m' = Map.insert (getCoordinates k) k m
