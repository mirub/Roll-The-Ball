{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where
import Data.Maybe
import Data.List

import ProblemState
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}



data Node s a = Node {state :: s,
                      action :: Maybe a,
                      parentNode :: Maybe (Node s a),
                      depth :: Int,
                      childNodes :: [Node s a]
                     } deriving (Ord, Show)

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState (Node state _ _ _ _) = state

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (Node _ _ parent _ _) = parent

nodeDepth :: Node s a -> Int
nodeDepth (Node _ _ _ nodeDepth _) = nodeDepth

nodeAction :: Node s a -> Maybe a
nodeAction (Node _ act _ _ _ ) = act

nodeChildren :: Node s a -> [Node s a]
nodeChildren (Node _ _ _ _ chl) = chl

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

-- Returneaza copiii unui nod
getChildren :: (ProblemState s a) => s -> a -> Node s a -> Int -> Node s a
getChildren lvl act p depth = newNode
    where 
        newNode = Node lvl (Just act) (Just p) depth succ
        succ = map (\(ac, lev) -> getChildren lev ac newNode (depth + 1)) children
        children = successors lvl

-- Creeaza nodul corespunzator unei stari
createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace lvl = node
    where
        node     = Node lvl Nothing Nothing 0 succ
        succ     = map (\(ac, lev) -> getChildren lev ac node 1) $ children
        children = successors lvl

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

instance (Eq s) => Eq (Node s a)
    where
        node1 == node2 = (nodeState node1) == (nodeState node2)

-- Implementarea logicii din spatele bfs-ului
bfsParser :: Ord s => [Node s a] -> [s] -> [([Node s a], [Node s a])]
bfsParser [] _ = []
bfsParser (x:queue) visited
    | length (x:queue) == 0 = []
    | (nodeState x) `elem` visited = bfsParser queue visited
    | otherwise = [(headChildren, newQueue)] ++ (bfsParser newQueue newVisited)
        where
            headChildren = filter (\v -> not(elem (nodeState x) visited)) (nodeChildren x)
            newQueue = queue ++ headChildren
            newVisited = (nodeState x):[] ++ visited

bfs :: Ord s => Node s a -> [([Node s a],[Node s a])]
bfs startingNode = bfsParser [startingNode] []

{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

-- Returneaza nodurile care aceeasi stare in cele 2 multimi
equalElems :: (Eq s) => [Node s a] -> [Node s a] -> [Node s a]
equalElems [] _ = []
equalElems (x:lst1) lst2
    | null new_elem == False = x : new_elem
    | otherwise = equalElems lst1 lst2
    where
        new_elem = foldr (\v acc -> if x == v then v:acc else acc) [] lst2

bidirParser :: (Eq s) => [([Node s a], [Node s a])] -> [([Node s a], [Node s a])] -> (Node s a, Node s a)
bidirParser [] _ = undefined
bidirParser _ [] = undefined
bidirParser (x:lst1) (y:lst2)
    | length eqElem /= 0 = (head eqElem, head (tail eqElem))
    | otherwise = bidirParser lst1 lst2
        where
            eqElem = (equalElems (fst x) (snd y)) ++ (equalElems (fst y) (snd x))

bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS start end = bidirParser (bfs start) (bfs end)
    
{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

getParents :: Node s a -> [Node s a]
getParents node
    | nodeDepth node == 0 = [node]
    | otherwise = node:[] ++ (getParents (fromJust(nodeParent node)))

extractPath :: Node s a -> [(Maybe a, s)]
extractPath node = map (\x -> (nodeAction x, nodeState x)) (reverse parents)
    where
        parents = getParents node

{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve = undefined
--solve initialState finalState = path
--    where
--        path = (extractPath firstHalf) ++ revAct
--        commonNode = bidirBFS (createStateSpace initialState) (createStateSpace finalState)
--        firstHalf = fst commonNode
--        secondHalf = snd commonNode
--        revAct = map (\entry@(action, state) -> (fromJust (fst (reverseAction (action, state))), snd(reverseAction (action, state)))) (extractPath secondHalf)

