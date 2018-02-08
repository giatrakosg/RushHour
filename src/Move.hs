module Move
    ( Move , Direction
    ) where


type CarType = Char
data Direction = North | South | East | West deriving (Show)

-- A move is a list of CarType and Directions that the Car
-- is moved towards

type Move = [(CarType,Direction)]
