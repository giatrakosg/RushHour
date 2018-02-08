module GameState
    ( State , getDir , getType
    ) where
import Data.Char
import Move
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


type CarSize = Int
-- Color of the car
type CarType = Char
-- Cartesian Coordinates of the Car
type CartCoord = (Int,Int)

data Orientation = UpDir | RightDir deriving (Show)

-- We store as values of Map  the size and the orientation
type Element = (Orientation,CarSize,CarType)


data State = State Int Int (Map CartCoord Element)
                deriving (Show,Eq)


-- gets the Direction of the element
getDir::Element->Orientation
getDir (x,_,_) = x

--gets type of element
getType::Element->CarType
getType (_,_,x) = x
-- gets Size of element
getSize::Element->CarSize
getSize(_,x,_) = x
-- Adds element to State
