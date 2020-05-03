{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A
import Data.List

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = Cell {cellPos :: Position,
                  cellType :: Char
                 } deriving (Eq, Ord, Show)

-- Getter pentru char
getCellChar :: Cell -> Char
getCellChar (Cell _ character) = character   

-- Getter pentru pozitie
getCellPos :: Cell -> Position
getCellPos (Cell pos _) = pos

{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level {board :: (A.Array (Int, Int) Cell),
                    startPos :: Position,
                    endPos :: Position
                    } deriving (Eq, Ord)

-- Getter pentru board
getBoard :: Level -> (A.Array (Int, Int) Cell)
getBoard (Level board _ _) = board

-- Getter pentru pozitia de start
getStartPos :: Level -> Position
getStartPos (Level _ start _) = start

-- Getter pentru pozitia de final
getEndPos :: Level -> Position
getEndPos (Level _ _ end) = end

{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

-- Returneaza char-ul din matrice de la pozitia (i, j) daca j <= col, endl altfel
printElem :: (A.Array (Int, Int) Cell) -> Int -> Int -> Char
printElem board i j 
    | j <= columns = getCellChar $ board A.! (i, j)
    | otherwise    = endl
    where 
        columns = snd(snd(A.bounds board))

-- Instantiaza show adaugand '/n' la inceput si toate char-urile din matrice intr-o lista de Char
instance Show Level 
    where
        show lvl = 
            let board = getBoard lvl
            in endl : [printElem board i j | i <- [0..fst(snd(A.bounds board))], j <- [0..(snd(snd(A.bounds board)) + 1)]]  
{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

testArray = A.array ((0, 0), (1, 1)) [((0, 0), (Cell (0, 0) topLeft)),
                                      ((0, 1), (Cell (0, 1) emptyCell)),
                                      ((1, 0), (Cell (1, 0) startUp)),
                                      ((1, 1), (Cell (1, 1) emptyCell))]

testLevel = Level testArray (0, 0) (0, 0)

changeField arr = arr A.// [((1, 1), (Cell (1, 1) botLeft))]


-- Creeaza un array gol de dimensiunile cerute
emptyArray :: Position -> (A.Array (Int, Int) Cell)
emptyArray pos = A.array ((0, 0), (fst pos, snd pos)) []

-- Creeaza un level gol
emptyLevel :: Position -> Level
emptyLevel pos = 
    let arr = emptyArray pos
        lines = fst pos
        columns = snd pos
        initialisedArray = arr A.// [((i, j), (Cell (i, j) emptySpace)) | i <- [0..lines], j <- [0..columns]]
    in (Level initialisedArray (0, 0) pos)
    
{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

-- Verifica daca pozitia e valida
validPos :: Position -> Level -> Bool
validPos pos lvl
    | fst pos < 0                      = False
    | fst pos > (fst(getEndPos(lvl)))  = False
    | snd pos < 0                      = False
    | snd pos > (snd(getEndPos(lvl)))  = False
    | otherwise = True

-- Adauga fiecare tip de celula
addCell :: (Char, Position) -> Level -> Level
addCell (typeChar, pos) lvl
    | validPos pos lvl == False = lvl
    | getCellChar(getBoard(lvl) A.! pos) /= emptySpace = lvl
    | otherwise =
        let newBoard = getBoard(lvl) A.// [((fst pos, snd pos), (Cell (fst pos, snd pos) typeChar))]
        in (Level newBoard (getStartPos lvl) (getEndPos lvl))

{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
-- Creeaza un nou nivel
createLevel :: Position -> [(Char, Position)] -> Level
createLevel pos cellList = foldr addCell (emptyLevel pos) cellList 


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

-- Creeaza o lista de celule imutabile
immutablePos :: [Char]
immutablePos = [startUp, startDown, startLeft, startRight, winUp, winDown, winLeft, winRight]

-- Adauga celula goala
addEmptyCell :: (Char, Position) -> Level -> Level
addEmptyCell (emprtSpace, pos) lvl = 
    let newBoard = getBoard(lvl) A.// [((fst pos, snd pos), (Cell (fst pos, snd pos) emptySpace))]
    in (Level newBoard (getStartPos lvl) (getEndPos lvl))

-- Muta celula
moveCell :: Position -> Directions -> Level -> Level
moveCell pos North lvl
    | fst pos - 1 < 0                                               = lvl
    | (getCellChar $ getBoard(lvl) A.! newPos) /= emptySpace        = lvl
    | (getCellChar $ getBoard(lvl) A.! pos) `elem` immutablePos     = lvl
    | otherwise = 
        let currentChar = getCellChar $ getBoard(lvl) A.! pos
        in addEmptyCell (emptySpace, pos) (addCell (currentChar, newPos) lvl)
    where newPos = (fst pos - 1, snd pos)    

moveCell pos East lvl
    | snd pos + 1 > (snd(getEndPos(lvl)))                           = lvl
    | (getCellChar $ getBoard(lvl) A.! newPos) /= emptySpace        = lvl
    | (getCellChar $ getBoard(lvl) A.! pos) `elem` immutablePos     = lvl
    | otherwise = 
        let currentChar = getCellChar $ getBoard(lvl) A.! pos
        in addEmptyCell (emptySpace, pos) (addCell (currentChar, newPos) lvl)
    where newPos = (fst pos, snd pos + 1)  
    
moveCell pos South lvl
    | fst pos + 1 > (fst(getEndPos(lvl)))                           = lvl
    | (getCellChar $ getBoard(lvl) A.! newPos) /= emptySpace        = lvl
    | (getCellChar $ getBoard(lvl) A.! pos) `elem` immutablePos     = lvl
    | otherwise = 
        let currentChar = getCellChar $ getBoard(lvl) A.! pos
        in addEmptyCell (emptySpace, pos) (addCell (currentChar, newPos) lvl)
    where newPos = (fst pos + 1, snd pos)    

moveCell pos West lvl
    | snd pos - 1 < 0                                               = lvl
    | (getCellChar $ getBoard(lvl) A.! newPos) /= emptySpace        = lvl
    | (getCellChar $ getBoard(lvl) A.! pos) `elem` immutablePos     = lvl
    | otherwise = 
        let currentChar = getCellChar $ getBoard(lvl) A.! pos
        in addEmptyCell (emptySpace, pos) (addCell (currentChar, newPos) lvl)
    where newPos = (fst pos, snd pos - 1)  

    {-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}

-- horPipe connections
horPipeEast :: [Char]
horPipeEast = [horPipe, botRight, topRight, startLeft, winLeft]

horPipeWest :: [Char]
horPipeWest = [horPipe, topLeft, botLeft, startRight, winRight]

-- verPipe connections
verPipeNorth :: [Char]
verPipeNorth = [verPipe, topLeft, topRight, startDown, winDown]

verPipeSouth :: [Char]
verPipeSouth = [verPipe, botLeft, botRight, startUp, winUp]

-- topLeft connections
topLeftEast :: [Char]
topLeftEast = [horPipe, topRight, startLeft, winLeft, botRight]

topLeftSouth :: [Char]
topLeftSouth = [verPipe, botLeft, startUp, winUp, botRight]

-- botLeft connections
botLeftNorth :: [Char]
botLeftNorth = [verPipe, topLeft, startDown, winDown, topRight]

botLeftEast :: [Char]
botLeftEast = [horPipe, botRight, startLeft, winLeft, topRight]

-- botRight connections
botRightNorth :: [Char]
botRightNorth = [verPipe, topRight, startDown, winDown, topLeft]

botRightWest :: [Char]
botRightWest = [horPipe, botLeft, startRight, winRight, topLeft]

-- topRight connections
topRightWest :: [Char]
topRightWest = [horPipe, topLeft, startRight, winRight, botLeft]

topRightSouth :: [Char]
topRightSouth = [verPipe, botRight, startUp, winUp, botLeft]

-- startUp connections
startUpNorth :: [Char]
startUpNorth = [verPipe, topLeft, topRight, winDown]

-- startDown connections
startDownSouth :: [Char]
startDownSouth = [verPipe, botLeft, botRight, winUp]

-- startLeft connections
startLeftWest :: [Char]
startLeftWest = [horPipe, topLeft, botLeft, winRight]

-- startRight connection
startRightEast :: [Char]
startRightEast = [horPipe, botRight, topRight, winLeft]

-- Directia North
upPos :: Position
upPos = (-1, 0)

-- Directia East
rightPos :: Position
rightPos = (0, 1)

-- Directia South
downPos :: Position
downPos = (1, 0)

-- Directia East
leftPos :: Position
leftPos = (0, -1)

-- Verifica daca pozInit + directie == pozFinal
isPositionEqual :: Position -> Position -> Position -> Bool
isPositionEqual firstPos secondPos direction = ((fst(firstPos) + fst(direction) == fst(secondPos)) &&
                                                    (snd(firstPos) + snd(direction) == snd(secondPos)))

-- Realizeaza conexiunile
connection :: Cell -> Cell -> Directions -> Bool
connection firstCell secondCell dir
    | ((firstPipe == horPipe) && (secondPipe `elem` horPipeEast) && (dir == East) && (isPositionEqual firstPos secondPos rightPos)) = True
    | ((firstPipe == horPipe) && (secondPipe `elem` horPipeWest) && (dir == West)) && (isPositionEqual firstPos secondPos leftPos) = True
    | ((firstPipe == verPipe) && (secondPipe `elem` verPipeNorth) && (dir == North)) && (isPositionEqual firstPos secondPos upPos) = True
    | ((firstPipe == verPipe) && (secondPipe `elem` verPipeSouth) && (dir == South)) && (isPositionEqual firstPos secondPos downPos) = True
    | ((firstPipe == topLeft) && (secondPipe `elem` topLeftEast) && (dir == East)) && (isPositionEqual firstPos secondPos rightPos) = True
    | ((firstPipe == topLeft) && (secondPipe `elem` topLeftSouth) && (dir == South)) && (isPositionEqual firstPos secondPos downPos) = True
    | ((firstPipe == botLeft) && (secondPipe `elem` botLeftNorth) && (dir == North)) && (isPositionEqual firstPos secondPos upPos) = True
    | ((firstPipe == botLeft) && (secondPipe `elem` botLeftEast) && (dir == East)) && (isPositionEqual firstPos secondPos rightPos) = True
    | ((firstPipe == botRight) && (secondPipe `elem` botRightNorth) && (dir == North)) && (isPositionEqual firstPos secondPos upPos) = True
    | ((firstPipe == botRight) && (secondPipe `elem` botRightWest) && (dir == West)) && (isPositionEqual firstPos secondPos leftPos) = True
    | ((firstPipe == topRight) && (secondPipe `elem` topRightWest) && (dir == West)) && (isPositionEqual firstPos secondPos leftPos) = True
    | ((firstPipe == topRight) && (secondPipe `elem` topRightSouth) && (dir == South)) && (isPositionEqual firstPos secondPos downPos) = True
    | ((firstPipe == startUp) && (secondPipe `elem` startUpNorth) && (dir == North)) && (isPositionEqual firstPos secondPos upPos) = True
    | ((firstPipe == startDown) && (secondPipe `elem` startDownSouth) && (dir == South)) && (isPositionEqual firstPos secondPos downPos)= True 
    | ((firstPipe == startLeft) && (secondPipe `elem` startLeftWest) && (dir == West)) && (isPositionEqual firstPos secondPos leftPos) = True
    | ((firstPipe == startRight) && (secondPipe `elem` startRightEast) && (dir == East)) && (isPositionEqual firstPos secondPos rightPos)  = True
    | otherwise = False
    where firstPipe = getCellChar $ firstCell
          secondPipe = getCellChar $ secondCell
          firstPos = getCellPos $ firstCell
          secondPos = getCellPos $ secondCell

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

-- Gaseste celula de start
findStartingCell :: Level -> Cell
findStartingCell lvl = (Cell pos typeCell)
    where pos = head [(i, j) | i <- [0..fst(snd(A.bounds board))], j <- [0..snd(snd(A.bounds board))],
                    (getCellChar (board A.! (i, j)) `elem` startCells)]
          board = getBoard(lvl)          
          typeCell = getCellChar $ getBoard(lvl) A.! pos        
          
-- Gaseste celula de final
findEndingCell :: Level -> Cell
findEndingCell lvl = (Cell pos typeCell)
    where pos = head [(i, j) | i <- [0..fst(snd(A.bounds board))], j <- [0..snd(snd(A.bounds board))],
                    (getCellChar (board A.! (i, j)) `elem` winningCells)]
          board = getBoard(lvl)          
          typeCell = getCellChar $ getBoard(lvl) A.! pos             

-- Functie care aduna 2 pozitii
addPositions :: Position -> Position -> Position
addPositions pos1 pos2 = (fst pos1 + fst pos2, snd pos1 + snd pos2)

-- Lista de directii posibile
possibleDirections :: [Position]
possibleDirections = [upPos, rightPos, downPos, leftPos]

-- Baga vecinul de sus intr-o lista daca e valid
getNorthNeigh :: Position -> Level -> [Position]
getNorthNeigh pos lvl
    | (fst(pos) - 1 >= 0) && (connection firstCell secondCell North) = (fst(pos) - 1, snd(pos)) : []
    | otherwise            = []
        where firstCell = (getBoard(lvl) A.! pos)
              secondCell = (getBoard(lvl) A.! (fst(pos) - 1, snd(pos)))

-- Baga vecinul de jos intr-o lista daca e valid
getSouthNeigh :: Position -> Level -> [Position]
getSouthNeigh pos lvl
    | (fst(pos) + 1 <= maxLines) && (connection firstCell secondCell South)  = (fst(pos) + 1, snd(pos)) : []
    | otherwise                    = []
        where maxLines = fst(getEndPos(lvl))
              firstCell = (getBoard(lvl) A.! pos)
              secondCell = (getBoard(lvl) A.! (fst(pos) + 1, snd(pos)))

-- baga vecinul din dreapta intr-o lista daca e valid
getEastNeigh :: Position -> Level -> [Position]
getEastNeigh pos lvl
    | (snd(pos) + 1 <= maxCol) && (connection firstCell secondCell East) = (fst(pos), snd(pos) + 1) : []
    | otherwise                   = []
        where maxCol = snd(getEndPos(lvl))
              firstCell = (getBoard(lvl) A.! pos)
              secondCell = (getBoard(lvl) A.! (fst(pos), snd(pos) + 1))

-- Baga vecinul din stanga intr-o lista daca e valid
getWestNeigh :: Position -> Level -> [Position]
getWestNeigh pos lvl
    | (snd(pos) - 1 >= 0) && (connection firstCell secondCell West) = (fst(pos), snd(pos) - 1) : []
    | otherwise                   = []
        where firstCell = (getBoard(lvl) A.! pos)
              secondCell = (getBoard(lvl) A.! (fst(pos), snd(pos) - 1))

-- Afla vecinii valizi elementului de pe pozitia pos
getValidNeigh :: Position -> Level -> [Position]
getValidNeigh pos lvl = [] ++ (getNorthNeigh pos lvl) ++ (getSouthNeigh pos lvl) ++ (getEastNeigh pos lvl) ++ (getWestNeigh pos lvl)

-- Helper pentru functia de wonLevel
wonLevelHelper :: Level -> Position -> Position -> Bool
wonLevelHelper lvl pos prevPos
    | (fst(pos) == fst(endPos)) && (snd(pos) == snd(endPos)) = True
    | length(nextNeigh) == 0 = False
    | otherwise = wonLevelHelper lvl nextPos pos
        where endPos = getCellPos(findEndingCell lvl)
              nextNeigh = delete prevPos (getValidNeigh pos lvl)
              nextPos = head(nextNeigh)

-- Verifica daca nivelul curent este castigator
wonLevel :: Level -> Bool
wonLevel lvl = wonLevelHelper lvl startPos startPos
    where startPos = getCellPos(findStartingCell lvl)

-- Vede ce actiuni se pot face din pozitia pos pe directia dir
getDoAction :: Position -> Level -> Directions -> [(Position, Directions)]
getDoAction pos lvl dir
    | (getCellChar(getBoard(lvl) A.! pos) `elem` immutablePos) = []
    | (getCellChar(getBoard(lvl) A.! pos) == emptySpace) = []
    | (fst(pos) - 1 >= 0) && (dir == North) && (getCellChar(getBoard(lvl) A.! northPos) == emptySpace) = [(pos, North)]
    | (fst(pos) + 1 <= fst(getEndPos(lvl))) && (dir == South) && (getCellChar(getBoard(lvl) A.! southPos) == emptySpace) = [(pos, South)]
    | (snd(pos) - 1 >= 0) && (dir == West) && (getCellChar(getBoard(lvl) A.! westPos) == emptySpace) = [(pos, West)]
    | (snd(pos) + 1 <= snd(getEndPos(lvl))) && (dir == East) && (getCellChar(getBoard(lvl) A.! eastPos) == emptySpace) = [(pos, East)]
    | otherwise = []
        where northPos = addPositions pos upPos
              southPos = addPositions pos downPos
              eastPos = addPositions pos rightPos
              westPos = addPositions pos leftPos

-- Returneaza actiunile pe toate directiile
getActions :: Position -> Level -> [(Position, Directions)]
getActions pos lvl = [] ++ (getDoAction pos lvl North) ++ (getDoAction pos lvl South) ++ (getDoAction pos lvl West) ++ (getDoAction pos lvl East)

-- Returneaza npua pozitie a pipe-ului vechi si directia de redo
getOldPos :: (Position, Directions) -> (Position, Directions)
getOldPos (pos, dir)
    | (dir == North) = ((fst(pos) - 1, snd(pos)), South)
    | (dir == South) = ((fst(pos) + 1, snd(pos)), North)
    | (dir == East) = ((fst(pos), snd(pos) + 1), West)
    | (dir == West) =  ((fst(pos), snd(pos) - 1), East)
    | otherwise = (pos, dir)


-- TEST MOVE CELL

testArray1 :: (A.Array (Int, Int) Cell)
testArray1 = A.array ((0, 0), (2, 2)) 
                    [((0, 0), (Cell (0,0) startDown)), ((0, 1), (Cell (0, 1) emptySpace)), ((0, 2), (Cell (0,2) emptySpace)),
                    ((1, 0), (Cell (1,0) emptySpace)), ((1, 1), (Cell (1,1 )verPipe)), ((1, 2), (Cell (1,2) emptySpace)),
                    ((2, 0), (Cell (2,0) winUp)), ((2, 1), (Cell (2,1) emptySpace)), ((2, 2), (Cell (2,2) emptySpace))]
               
test4 = moveCell (1, 1) North (Level (testArray1) (0,0) (2,2))


-- END TEST MOVE CELL

instance ProblemState Level (Position, Directions) where
    -- Iau toate piesele si vad unde ma pot muta cu ele
    successors lvl = map (\(pos,dir) -> ((pos,dir), moveCell pos dir lvl)) actionsList 
        where lines = fst(getEndPos(lvl))
              columns = snd(getEndPos(lvl))
              actionsList = concat [getActions (i, j) lvl | i <- [0..lines], j <- [0..columns]]          

    isGoal lvl = wonLevel lvl

    reverseAction action@((pos, dir), lvl) = (oldPos, oldLevel)
        where oldPos = getOldPos (pos, dir)
              newPlace = fst(oldPos)
              oldDir = snd(oldPos)
              oldLevel = moveCell newPlace oldDir lvl
                   