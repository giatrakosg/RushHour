import Data.Map.Strict as Map
import Data.Set as Set
import Data.Maybe
import Data.List as List
import Data.Ord

solve_astar :: State -> [Move]
solve_astar st = aStar (Main.fromList [initState st]) (Set.fromList []) heuristic isFinalNode

aStar:: (PairingHeap Node) -- Heap of Open nodes
        -> (Set Node) -- Set of seen nodes
        -> (State -> Int) -- Heuristic that calculates cost of moving to that node
        -> (Node -> Bool) -- Function that returns true if desired node
        -> [Move] -- Result is a list of moves (In case of no solution returns error )

aStar open closed heuristic isFinalFn
                | (Main.isEmpty open) = error "Unsolveable Puzzle " -- No Nodes to search
                | isFinalFn current = stepsTaken current -- Found final node
                | isntNew current = aStar open' closed heuristic isFinalFn -- If already seen discard
                | otherwise = aStar open'' closed' heuristic isFinalFn -- Expand current node , and place
                -- the result in the closed set
                where
                    current = Main.findMin open -- Current node is cheapest in Heap
                    isntNew current = current `Set.member` closed
                    open' = Main.deleteMin open -- Delete current from Heap of open
                    closed' = Set.insert current closed -- Insert current to closed
                    neighbors = nodeexpand current closed heuristic -- Find neighbours of current
                    open'' = visit neighbors open' -- Adds the neighbor nodes to the open set

finalState::State->Bool
finalState (State len _ ms _) = (xC,yC) `elem` expnd
                                    where
                                        keyElm = ms ! '='
                                        expnd = expand keyElm
                                        xC = fst (expnd !! 0)
                                        yC = len

-- Direction a car move towards
data Direction = North | South | East | West deriving (Show,Eq)

-- A move is a list of CarType and Directions that the Car
-- is moved towards , and how many tiles
type Positions = Int
type Move = (CarType,Direction,Positions)

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

-- State Length Width (Map CarType(=Key) Element) (Set Positions occupied ,by )
data State = State Int Int (Map Key Element) (Set Int)
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
newVal::Element->Direction->Positions->Element
newVal (c,d,(x,y)) a pss
    | a == North = (c,d,(x-pss,y))
    | a == South = (c,d,(x+pss,y))
    | a == East  = (c,d,(x,y+pss))
    | a == West  = (c,d,(x,y-pss))

makeMove::State->Move->State
makeMove (State len wid ms ss) (tp,dir,pss) = (State len wid ms' ss'')
                            where
                                oldVal =  ms ! tp
                                expnd = expand oldVal
                                ss' = List.foldl' (\x y -> Set.delete (cart2norm len wid y) x) ss expnd -- Delete old used elements
                                val' = newVal oldVal dir pss
                                ms' = Map.insert tp val' ms
                                expnd' = expand val'
                                ss'' = List.foldl' (\x y -> Set.insert (cart2norm len wid y) x) ss' expnd'



index::String->Char->Int
index [] _ = 1
index (a:str) c = if a == c
    then 1
    else 1 + (index str c)

getCarStartNorm::String->Char->Int
getCarStartNorm str c = index (clean str) c
-- Found at Chris Okasaki
-- Purely Functional Data Structures
-- page 210 (Edition 1998)



class Heap h where
    empty     :: (Ord a) => h a
    isEmpty   :: (Ord a) => h a -> Bool

    insert    :: (Ord a) => a   -> h a -> h a
    merge     :: (Ord a) => h a  -> h a -> h a
    findMin   :: (Ord a) => h a -> a
    deleteMin :: (Ord a) => h a -> h a


-- How far away from the end is the car
pos2end::State->Int
pos2end (State len _ ms _) = len - (snd lst)
                                where
                                    target = ms ! '='
                                    expnd = expand target
                                    lst = last expnd -- Furthest to the right

-- How many cars are in front of '='
carsfront::State->Int
carsfront (State len wid ms ss) = Set.size ss'
                                     where
                                         crt' =  ms ! '=' -- Position of '='
                                         crt = last $ expand crt'
                                         xC = fst crt
                                         end = (xC,len)
                                         minR = cart2norm len wid crt
                                         maxR = cart2norm len wid end
                                         ss' = Set.intersection (Set.fromList [(minR + 1)..maxR]) ss




heuristic::State->Int
heuristic st = (carsfront st) + (pos2end st)

getVertNorms::Int->Int->Int->[Int]
getVertNorms i len wid = [i + (j*len) | j <- [0..(wid - 1)]]

getHorNorms::Int->Int->Int->[Int]
getHorNorms i len wid = [((i-1)*len) + j + 1| j <- [0..(len -1 )]]

-- Returns all squares up / down of car type or left / right
neighbours::State->CarType->Orientation->[CartCoord]
neighbours (State len wid ms ss) tp UpDir = up ++ down
                                            where
                                                elm = ms ! tp -- The element to move
                                                expnSet = Set.fromList $ List.map (cart2norm len wid) $ expand elm
                                                ss' = Set.difference ss expnSet
                                                (x0,y0) = trd3 elm -- Coordinates of element
                                                nrmCrt = cart2norm len wid (x0,y0)
                                                sz = snd3 elm -- Size of element
                                                vertSet = Set.fromList $ getVertNorms y0 len wid
                                                ss'' = Set.intersection ss' vertSet
                                                dirAbove = Set.lookupLT nrmCrt ss'' -- Directly Above
                                                dirBellow = Set.lookupGT nrmCrt ss'' -- Directly Below
                                                (xU,yU) = if dirAbove == Nothing
                                                    then (1,y0)
                                                    else (x'+1,y')
                                                        where
                                                            (x',y') = norm2cart len wid (fromJust dirAbove)
                                                (xD,yD) = if dirBellow == Nothing
                                                    then (wid - sz + 1 ,y0)
                                                    else (x' - sz ,y')
                                                        where
                                                            (x',y') = norm2cart len wid (fromJust dirBellow)

                                                up = [(x,y0) | x <- [xU..(x0-1)]]
                                                down = [(x,y0) | x <- [(x0+1)..xD]]

neighbours (State len wid ms ss) tp RightDir = left ++ right
                                            where
                                                elm = ms ! tp -- The element to move
                                                expnSet = Set.fromList $ List.map (cart2norm len wid) $ expand elm
                                                ss' = Set.difference ss expnSet
                                                (x0,y0) = trd3 elm -- Coordinates of element
                                                nrmCrt = cart2norm len wid (x0,y0)
                                                sz = snd3 elm -- Size of element
                                                horSet = Set.fromList $ getHorNorms x0 len wid
                                                ss'' = Set.intersection ss' horSet -- Occupied tiles in line
                                                dirLeft = Set.lookupLT nrmCrt ss'' -- Directly Above
                                                dirRight = Set.lookupGT nrmCrt ss'' -- Directly Below
                                                (xU,yU) = if dirLeft == Nothing -- Nothing above
                                                    then (x0,1)
                                                    else (x',y'+1) -- Someting left
                                                        where
                                                        (x',y') = norm2cart len wid (fromJust dirLeft)
                                                (xD,yD) = if dirRight == Nothing
                                                    then (x0,len - sz + 1)
                                                    else  (x',y' - sz )
                                                        where
                                                            (x',y') = norm2cart len wid (fromJust dirRight)
                                        --        (xU,yU) = norm2cart len wid dirAbove
                                        --        (xD,yD) = norm2cart len wid dirBellow
                                                left = [(x0,y) | y <- [yU..(y0-1)]]
                                                right = [(x0,y) | y <- [(y0+1)..yD]]



inborder::State->CartCoord->Bool
inborder (State len wid _ _) (x,y) = ((0 < x) && (x <= wid)) && ((0 < y) && (y <= len))
isempty::State->CartCoord->CarType->Bool
isempty (State len wid ms ss) (x,y) tp = not (crt `Set.member` ss')
                                  where
                                      elm = ms ! tp
                                      expnSet = Set.fromList $ List.map (cart2norm len wid) $ expand elm
                                      ss' = Set.difference ss expnSet
                                      crt = cart2norm len wid (x,y)

nocollision::State->CartCoord->CarType->Bool
nocollision st@(State len wid ms ss) to tp = if (Set.size ss'' == 0)
    then True
    else False
                                             where
                                                 elm = ms ! tp
                                                 ori = fst3 elm
                                                 sz = snd3 elm
                                                 newEl = (ori,sz,to)
                                                 exp' = expand newEl
                                                 expSet' = Set.fromList $ List.map (cart2norm len wid ) exp'
                                                 expn = Set.fromList $ List.map (cart2norm len wid) $ expand elm -- previous positions
                                                 ss' = Set.difference ss expn
                                                 ss'' = Set.intersection ss' expSet'

isvalid::State->CartCoord->CarType->Bool
isvalid st1@(State len wid _ _) (x,y) tp = (isempty st1 (x,y) tp) && (inborder st1 (x,y))
                                        --    && (nocollision st1 (x,y) tp)


-- Returns all valid move positions of given CarType
-- Finds the neighboring cells of tp and filters them by validity
carMoves::State->CarType->[CartCoord]
carMoves st1@(State len wid ms _) tp = nbr
    --List.filter (\x -> isvalid st1 x tp) nbr
                                 where
                                     nbr = neighbours st1 tp (fst3 $ ms ! tp )

turn2move::State->CarType->CartCoord->Move
turn2move st1@(State len wid ms _) tp crt
                                         | oldPos == crt = error "Invalid Move"
                                         | fst oldPos < fst crt = (tp,South,(fst crt) - (fst oldPos))
                                         | fst oldPos > fst crt = (tp,North,(fst oldPos) - (fst crt))
                                         | snd oldPos < snd crt = (tp,East ,(snd crt) - (snd oldPos))
                                         | snd oldPos > snd crt = (tp,West ,(snd oldPos) - (snd crt))
                                         where
                                             oldPos = trd3 $ ms ! tp -- Old position

moveList::State->CarType->[CartCoord]->[Move]
moveList  _   _  [] = []
moveList st1 tp ls = List.map (\x -> (turn2move st1 tp x)) ls

deepMoveList::State->[CarType]->[[CartCoord]]->[[Move]]
deepMoveList _ [] _ = []
deepMoveList st1 (t:tp) (l:ls) = if List.null l
    then deepMoveList st1 tp ls
    else (moveList st1 t l) : (deepMoveList st1 tp ls)

successorMoves::State->[(Move,Int)]
successorMoves st1@(State len wid ms _) = List.zip (List.concat $ deepMoveList st1 keys crtMvs) [1,1..]
                                        where
                                            -- list of keys
                                            keys = Map.keys ms
                                            -- list of valid moves for each car type
                                            crtMvs = List.map (\x -> carMoves st1 x) keys


data Node = Root
            | Node { state :: State ,
                     moves :: [Move] ,
                     gScore :: Int ,
                     hScore :: Int ,
                     prev :: Node }
                    deriving (Show)

instance Eq Node where
    m == n = (state m ) == (state n)
instance Ord Node where
    n       <= Root = False
    Root <= n = True
    m <= n = ((gScore m) + (hScore m)) <= ((gScore n) + (hScore n))

initState :: State -> Node
initState st = (Node st [] 0 0 Root)

makeMoves::State->[Move]->State
makeMoves st ls = List.foldl' makeMove st ls

-- Given a node and the empty list , returns the state
getState::Node->[Move]->State
getState nd _ = state nd

-- Function that given a node and a heuristic returns list of nodes
succNodes::Node->(State->Int)->[Node]
succNodes (Root) heuristic = []
succNodes nd heuristic = List.map (\(x,y) -> Node (makeMove state' x)
                                                  ((moves nd) ++ [x])
                                                  (y + (gScore nd))
                                                  (heuristic (makeMove state' x))
                                                  nd)
                                                  (successorMoves state')
                            where
                                state' = state nd

-- Returns list of Moves from Root State
stepsTaken::Node->[Move]
stepsTaken (Root ) = []
stepsTaken m    = moves m

isFinalNode::Node->Bool
isFinalNode nd = finalState $ state nd


visit :: [Node] -> (PairingHeap Node) -> (PairingHeap Node)
visit [] open = open
visit (n:ns) open
    | isNewNeighbor     = visit ns (Main.insert n open) -- If it is new add it to the list of open
    | otherwise         = visit ns open -- If it not new , or cheaper to get to the State then continue
    where
        isNewNeighbor     = not (n `elemHeap` open) -- State doesn't exist


-- Expand all neighbor nodes to node that are not already in closed
-- By using a Set we ensure that we don't have duplicates
nodeexpand :: Node -> (Set Node) -> (State -> Int) ->[Node]
nodeexpand node closed heuristic = List.filter (\x -> not (x `List.elem` closedls)) nodes
                    where
                        nodes = succNodes node heuristic
                        closedls = Set.toList closed



-- Found at Chris Okasaki
-- Purely Functional Data Structures
-- page 210 (Edition 1998)

-- There are extensions

data PairingHeap a = E | T a [PairingHeap a] deriving (Show)

mergePairs [] = E
mergePairs [h] = h
mergePairs (h1:h2:hs) = merge (merge h1 h2) (mergePairs hs)

instance Heap PairingHeap where
    empty = E
    isEmpty E = True
    isEmpty _ = False

    insert x h = merge (T x []) h

    merge h E = h
    merge E h = h
    merge h1@(T x hs1) h2@(T y hs2) =
        if x < y
            then T x (h2:hs1)
            else T y (h1:hs2)

    findMin E = error "empty heap"
    findMin (T x _) = x

    deleteMin E        = E
    deleteMin (T _ hs) = mergePairs hs

-- EXTENSIONS TO OKASAKI IMPLEMENTATION
fromList::(Ord a)=>[a]->PairingHeap a
fromList [] = E
fromList (x:xs) = Main.insert x (Main.fromList xs)

elemHeap::(Eq a)=>a->(PairingHeap a)->Bool
elemHeap _ (E)     = False
elemHeap x (T y []) = x == y
elemHeap x (T y (h:hs)) = if x == y
    then True
    else or (h' : hs')
            where
                hs' = List.map (elemHeap x) hs
                h'  = elemHeap x h

toList::(PairingHeap a) -> [a]
toList E = []
toList (T x []) = [x]
toList (T x (h:hs)) = (x : hl) ++ rs'
                        where
                            rs = List.map Main.toList hs -- Pass through list list of children
                            rs' = concat rs
                            hl = Main.toList h



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


splitTillEmpty::[a]->Int->[[a]]
splitTillEmpty [] _ = [[]]
splitTillEmpty ys n = lft : (splitTillEmpty rgt n)
                        where
                            tup = List.splitAt n ys
                            lft = fst tup
                            rgt = snd tup

-- Adds the char c every x'th char of String
addCharEvery::String->Char->Int->String
addCharEvery str c x = List.concat withBreak
                        where
                            splat = splitTillEmpty str x
                            withBreak = List.map (\x -> x ++ [c]) splat
-- Write State works as follows :
-- 1. Find list of expanded elements (see expand)
-- 2. Convert to normal coordinates
-- 3. Find all positions not occupied by the expanded elements
-- 4. Make a list ('.',x) where x member of set of unused positions
-- 5. Append the lists and sort them by normal coordinates
-- 6. Add 'n' at every length element
-- 7. Print one after the other

writeState::State->String
writeState (State len width ms _) = init $ init $ addCharEvery (List.map fst srtPos) '\n' len
                                    where
                                        ls = Map.toList ms -- list of (keys,element)
                                        elems = List.map snd ls -- list of elements
                                        keys = List.map fst ls -- list of keys
                                        expElems = List.map expand elems -- list of expanded elems
                                        normExpElems = List.map ((\x -> List.map (cart2norm len width ) x )) expElems -- Normalized
                                        flatExp = List.concat normExpElems
                                        -- Normal coordinates of empty cells
                                        -- List of all norm position not occupied
                                        dotPos = [x | x <- [1..(width * len)] , not (x `elem` flatExp)]
                                        -- Turn to tuple ('.',Position)
                                        dots = tuplify '.' dotPos
                                        -- Turn to [[('a',Pos1),..],[('b',Pos1)..]]
                                        expndElems = deeptuples keys normExpElems
                                        allPos = expndElems ++ [dots]
                                        flatPos = List.concat allPos
                                        srtPos = List.sortBy (comparing snd) flatPos
