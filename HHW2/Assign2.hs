import Data.List

--1 
data SudokuConfig = SudokuConfig [Integer]

--2
sudokuConfigFromList lst = (SudokuConfig lst)
--sudokuConfigFromList listExample

--  5 1 3   6 8 7   2 4 9 
--  8 4 9   5 2 1   6 3 7 
--  2 6 7   3 4 9   5 8 1 

--  1 5 8   4 6 3   9 7 2 
--  9 7 4   2 1 8   3 6 5 
--  3 2 6   7 9 5   4 1 8 

--  7 8 2   9 3 4   1 5 6 
--  6 3 5   1 7 2   8 9 4 
--  4 9 1   8 5 6   7 2 3 

--sudokuConfigFromList trivial

--  _ 4 6   _ _ _   8 9 _ 
--  _ 7 _   4 _ 9   _ 1 _ 
--  5 _ _   _ 8 _   _ _ 6 

--  _ _ 3   9 _ 8   6 _ _ 
--  9 _ _   _ _ _   _ _ 2 
--  _ _ 8   5 _ 2   1 _ _ 

--  4 _ _   _ 5 _   _ _ 3 
--  _ 2 _   1 _ 6   _ 7 _ 
--  _ 9 7   _ _ _   5 2 _ 

--3
listFromSudokuConfig (SudokuConfig lst) = lst
--listFromSudokuConfig (SudokuConfig [1..20])
--[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
--
-- listFromSudokuConfig (SudokuConfig trivial)
--[0,4,6,0,0,0,8,9,0,0,7,0,4,0,9,0,1,0,5,0,0,0,8,0,0,0,6,0,0,3,9,0,8,6,0,0,9,0,0,0,0,0,0,0,2,0,0,8,5,0,2,1,0,0,4,0,0,0,5,0,0,0,3,0,2,0,1,0,6,0,7,0,0,9,7,0,0,0,5,2,0]
--4
instance Eq SudokuConfig where
	config1 == config2 = 
					(listFromSudokuConfig config1 == listFromSudokuConfig config2)
--5
--to display the sudoku puzzle in systematical order
instance Show SudokuConfig where
  show (SudokuConfig grid) = "\n"++printThis grid 0
             where 
             	  printThis [] _ = []
                  printThis lst 3 = "\n"++printThis lst 0
                  printThis lst n = (rowBlock (take 9 lst) " " 0) ++"\n"++ 
                                   printThis (drop 9 lst) (n + 1)
                  rowBlock [] str _     = str ++ " "
                  rowBlock x  str 3     = rowBlock x (str ++ "  ") 0
                  rowBlock (x:xs) str n = rowBlock xs (str ++ " " ++ (if x == 0 then "_" else show x)) (n + 1)


--Just (sudokuConfigFromList trivial)
--Just (sudokuConfigFromList listExample)

--6
instance Config SudokuConfig where
  successors (SudokuConfig []) = []
  successors (SudokuConfig puzzle) = 
    [SudokuConfig (replace (findZeroPos puzzle 0) a puzzle)|a <- (possibleValues (findZeroPos puzzle 0) puzzle)]
--successors (SudokuConfig trivial)
--[
--  1 4 6   _ _ _   8 9 _ 
--  _ 7 _   4 _ 9   _ 1 _ 
--  5 _ _   _ 8 _   _ _ 6 

--  _ _ 3   9 _ 8   6 _ _ 
--  9 _ _   _ _ _   _ _ 2 
--  _ _ 8   5 _ 2   1 _ _ 

--  4 _ _   _ 5 _   _ _ 3 
--  _ 2 _   1 _ 6   _ 7 _ 
--  _ 9 7   _ _ _   5 2 _ 
--,
--  2 4 6   _ _ _   8 9 _ 
--  _ 7 _   4 _ 9   _ 1 _ 
--  5 _ _   _ 8 _   _ _ 6 

--  _ _ 3   9 _ 8   6 _ _ 
--  9 _ _   _ _ _   _ _ 2 
--  _ _ 8   5 _ 2   1 _ _ 

--  4 _ _   _ 5 _   _ _ 3 
--  _ 2 _   1 _ 6   _ 7 _ 
--  _ 9 7   _ _ _   5 2 _ 
--,
--  3 4 6   _ _ _   8 9 _ 
--  _ 7 _   4 _ 9   _ 1 _ 
--  5 _ _   _ 8 _   _ _ 6 

--  _ _ 3   9 _ 8   6 _ _ 
--  9 _ _   _ _ _   _ _ 2 
--  _ _ 8   5 _ 2   1 _ _ 

--  4 _ _   _ 5 _   _ _ 3 
--  _ 2 _   1 _ 6   _ 7 _ 
--  _ 9 7   _ _ _   5 2 _ 
--]


--SudokuConfig listExample

--  5 1 3   6 8 7   2 4 9 
--  8 4 9   5 2 1   6 3 7 
--  2 6 7   3 4 9   5 8 1 
--
--  1 5 8   4 6 3   9 7 2 
--  9 7 4   2 1 8   3 6 5 
--  3 2 6   7 9 5   4 1 8 
--
--  7 8 2   9 3 4   1 5 6 
--  6 3 5   1 7 2   8 9 4 
--  4 9 1   8 5 6   7 2 3 


--7
isSudokuGoal :: SudokuConfig -> Bool
isSudokuGoal (SudokuConfig lst) = (rowIsFine 0 lst) && (columnsFine 0 lst) && (blocksFine 0 lst)

--isSudokuGoal (SudokuConfig problem)
--False
--isSudokuGoal (SudokuConfig listExample)
--True

--8
sudokuSolve :: SudokuConfig -> Maybe SudokuConfig
sudokuSolve (SudokuConfig puzzleList) = solve isSudokuGoal (SudokuConfig puzzleList)
--sudokuSolve (sudokuConfigFromList  problem)
--sudokuSolve (sudokuConfigFromList profi)
--sudokuSolve (sudokuConfigFromList  trivial)
--Just 
--  1 4 6   3 2 5   8 9 7 
--  8 7 2   4 6 9   3 1 5 
--  5 3 9   7 8 1   2 4 6 
--
--  2 1 3   9 7 8   6 5 4 
--  9 5 4   6 1 3   7 8 2 
--  7 6 8   5 4 2   1 3 9 

--  4 8 1   2 5 7   9 6 3 
--  3 2 5   1 9 6   4 7 8 
--  6 9 7   8 3 4   5 2 1 

--sudokuSolve (sudokuConfigFromList profi)
--Just 
--  1 5 7   2 6 9   8 3 4 
--  6 9 3   8 4 1   2 5 7 
--  8 4 2   3 7 5   1 9 6 
--
--  5 1 6   4 8 2   3 7 9 
--  4 7 9   5 1 3   6 8 2 
--  2 3 8   6 9 7   4 1 5 
--
--  9 8 1   7 2 6   5 4 3 
--  7 2 5   1 3 4   9 6 8 
--  3 6 4   9 5 8   7 2 1 
-------------------------------------------------------------------------------------------------------------------
class (Eq c, Show c) => Config c where
  successors :: c -> [c]

solveAll :: (Config c) => (c -> Bool) -> c -> [c]
solveAll isGoal c = let restSolutions = concat [solveAll isGoal c' | c' <- successors c] 
                    in if isGoal c then c:restSolutions else restSolutions

solve :: (Config c) => (c -> Bool) -> c -> (Maybe c)
solve isGoal c = case solveAll isGoal c of
                   []   -> Nothing
                   x:xs -> Just x

-----------------------------------------------helpers----------------------------------------------------------------------
--gives the indices in of the column where the input index is present
getColumn :: Int -> [Int]
getColumn y = getColumns y 0
         where getColumns _ 9 = []
               getColumns y indx = y : getColumns (y + 9) (indx + 1)
--getColumn 0
--getColumn 1

--gives the indices present in the row x

getRow :: Int -> [Int]
getRow x = [s..(s+8)]
			where s = x * 9;
--getRow 5
--getRow 7

--gives the indices which are present in block where input is start index  start row  and start colum

getBlock:: (Num a1, Num t, Num a, Eq a1, Eq a) => t -> a1 -> a -> [t]
getBlock _ _ 3 =[]
getBlock x 3 z = getBlock (x+6) 0 (z+1)
getBlock x y z = x : getBlock (x+1) (y+1) z

--getBlock 0 0 0

--calls the get block with startindex of the block and 0 0 to start the block form a specificc pos
getBlockNumbers:: Int->[Int]
getBlockNumbers n = getBlock (getStartIndex n) 0 0
--getBlockNumbers 3
--getBlockNumbers 5

--gives the start index of a specific blocknumber
getStartIndex:: Int -> Int
getStartIndex y = if y <= 2
						then y * 3
					else if y <= 5
							then (y*3) + 18
						 else (y*3) + 36
--getStartIndex 4
--getStartIndex 0

--retrieves the element at the position index from the lst 
retriveElement :: Int -> [a] -> a
retriveElement position lst = last (take (position+1) lst) 
--retriveElement 0 listExample

--retrieves the elements at the position index (x:xs) in the given lst
retriveList :: [Int] -> [t] -> [t]
retriveList [] lst = []
retriveList (x:xs) lst = (retriveElement x lst) : (retriveList xs lst)
--retriveList [1..9] listExample
------------------------------------------helpers-------------------------------------------------------------------------
--takes the list and checks if it has all the elements from 0 to 9
rowIsFine :: (Ord a, Num a, Enum a) => Int -> [a] -> Bool
rowIsFine 9 lst = True
rowIsFine n lst = ((sort (retriveList (getRow n) lst)) == [1..9]) && (rowIsFine (n+1) lst)
--rowIsFine 0 listExample


--takes the list and checks if it has all the elements from 0 to 9
columnsFine :: (Ord a, Num a, Enum a) => Int -> [a] -> Bool
columnsFine 9 lst = True
columnsFine n lst = ((sort (retriveList (getColumn n) lst)) == [1..9]) && (columnsFine (n+1) lst)
--columnsFine 0 listExample

--takes the list and checks if it has all the elements from 0 to 9
blocksFine :: (Ord a, Num a, Enum a) => Int -> [a] -> Bool
blocksFine 9 lst = True
blocksFine n lst = ((sort (retriveList (getBlockNumbers n) lst)) == [1..9]) && (blocksFine (n+1) lst)
--blocksFine 4 listExample

------------------------------------------------------helper for checking the rows--------------------------------------------------------------
--gets the row number of the index
getRowNumber :: Integral a => a -> a
getRowNumber index = quot index 9
--getRowNumber 3

--gets the colum number based on index
getColumnNumber :: Integral a => a -> a
getColumnNumber index  = index `mod` 9
--getColumnNumber 5

--gets the block number based on index
getNOfBlock :: Int -> Int -> Int
getNOfBlock index start = if index `elem` (getBlockNumbers start ) then start else getNOfBlock index (start + 1)
--getNOfBlock50

------------------------------------------------helpers for sudokuSolve--------------------------------------------------------------------
--gets the values which are not possible for a index based on its row column and block
notPossibleValues :: Ord a => Int -> [a] -> [a]
notPossibleValues n puzzle = (sort (nub ((retriveList (getRow (getRowNumber n)) puzzle) ++ 
							 (retriveList (getColumn (getColumnNumber n)) puzzle) ++
							 (retriveList (getBlockNumbers (getNOfBlock n 0)) puzzle))))
--notPossibleValues 2 problem

--gets the possible values by removing the not possible values from [1..9]
possibleValues :: (Ord t, Num t, Enum t) => Int -> [t] -> [t]
possibleValues _ [] = []
possibleValues (-1) _ = []
possibleValues pos puzzle = remove [1..9] (notPossibleValues pos puzzle)
                   where remove [] _       = []
                         remove (x:xs) impossiblesValues = if (x `notElem` impossiblesValues) then x : removeNxt else removeNxt
                                           where removeNxt = remove xs impossiblesValues
--possibleValues 2 problem 
-------------------------------------------helper for sudokuSolve------------------------------------------------------------------------
--tells if list has a 0 or no
hasZero :: (Num a, Eq a) => [a] -> Bool
hasZero puzzle = if 0 `elem` puzzle then True else False
--hasZero problem
--hasZero listExample

--finds the first position where 0 occurs, if no zero will return -1
findZeroPos :: (Num a1, Num a, Eq a1, Eq a) => [a1] -> a -> a
findZeroPos _ 81 = -1
findZeroPos (z:zs) pos = if z == 0
							then pos 
						 else findZeroPos zs (pos+1)
--findZeroPos listExample 0

--replaces the nth index elment with the a in the list puzzle
replace :: Int -> a -> [a] -> [a]
replace n a puzzle =  (take n puzzle) ++ [a] ++ (drop (n+1) puzzle)
--replace 2 (-1) [1..9]
--replace 2 0 listExample

--------------------------------------examples-------------------------------------------------------------------------------
trivial =    [ 0, 4, 6,  0, 0, 0,  8, 9, 0,
               0, 7, 0,  4, 0, 9,  0, 1, 0,
               5, 0, 0,  0, 8, 0,  0, 0, 6,

               0, 0, 3,  9, 0, 8,  6, 0, 0,
               9, 0, 0,  0, 0, 0,  0, 0, 2,
               0, 0, 8,  5, 0, 2,  1, 0, 0,

               4, 0, 0,  0, 5, 0,  0, 0, 3,
               0, 2, 0,  1, 0, 6,  0, 7, 0,
               0, 9, 7,  0, 0, 0,  5, 2, 0 ]

profi =      [ 1, 0, 7,  0, 0, 9,  8, 0, 4,
               0, 0, 3,  0, 4, 0,  0, 0, 0,
               8, 0, 2,  0, 0, 5,  0, 0, 6,
            
               0, 0, 0,  0, 8, 0,  3, 0, 0,
               0, 0, 0,  5, 0, 0,  0, 0, 0,
               2, 3, 8,  0, 0, 0,  0, 1, 0,
            
               0, 8, 1,  0, 0, 6,  5, 0, 0,
               0, 2, 0,  0, 0, 4,  0, 0, 0,
               3, 0, 0,  9, 0, 8,  7, 0, 0 ];

problem =    [ 1, 0, 0,  0, 0, 0,  0, 0, 9,
               0, 5, 0,  0, 0, 0,  0, 2, 0,
               0, 0, 9,  0, 0, 0,  4, 0, 0,

               0, 0, 0,  5, 6, 7,  0, 0, 0,
               0, 0, 0,  8, 9, 1,  0, 0, 0,
               0, 0, 0,  2, 3, 4,  0, 0, 0,

               0, 0, 5,  0, 0, 0,  9, 0, 0,
               0, 7, 0,  0, 0, 0,  0, 4, 0,
               9, 0, 0,  0, 0, 0,  0, 0, 8 ];

listExample = [5, 1, 3, 6, 8, 7, 2, 4, 9, 8, 4, 9, 5, 2, 1, 6, 3, 7, 2, 6, 7, 3, 4, 9, 5, 8, 1, 1, 5, 8, 4, 6, 3, 9, 7, 2, 9, 7, 4, 2, 1, 8, 3, 6, 5, 3, 2, 6, 7, 9, 5, 4, 1, 8, 7, 8, 2, 9, 3, 4, 1, 5, 6, 6, 3, 5, 1, 7, 2, 8, 9, 4, 4, 9, 1, 8, 5, 6, 7, 2, 3]


---------------------------------------------------------------------------------------------------------------------
--------------------------------------question 2-------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------


--1
--this data type is either boolean/string/(and/or/not operation on datatype of its own kind) 
data BExp = BConst Bool|Var String|And BExp BExp|Or BExp BExp| Not BExp deriving Eq


constructor :: BExp -> [(String, Bool)] -> Bool
constructor (And a b) table = (constructor a table) && (constructor b table)
constructor (Or a b) table = (constructor a table) || (constructor b table)
constructor (Not a) table = not (constructor a table)
constructor (BConst a) table = a
constructor (Var a) table = findValue table a

--2
data SatConfig = SatConfig [(String, Bool)] [String] BExp deriving Eq

instance Show SatConfig where
	show(SatConfig tpls vars exp) = show(tpls)

instance Config SatConfig where
  successors (SatConfig tpls [] _) = []
  successors (SatConfig tpls (var:vars) exp) = 
    [(SatConfig ((var, b):tpls) vars exp)| b<- [True,False]]

--successors (SatConfig [("x",True),("y",False)] ["x"] (And (Var "x") (Var "y")))
--[[("x",True),("x",True),("y",False)],[("x",False),("x",True),("y",False)]]

--3
--this takes the satConfig and tests if it satisfies the BExpression
isGoal :: SatConfig -> Bool
isGoal (SatConfig tpls [] exp) = constructor exp tpls
isGoal (SatConfig tpls vars exp) = False
--isGoal (SatConfig [("x",True),("y",False)] [] (And (Var "x") (Var "y")))
--False

--4
--this solves the expression, which is its inpput
satSolve :: BExp -> Maybe SatConfig
satSolve vars =
	let sigma = findString vars
	in solve isGoal (SatConfig [] sigma vars)


--satSolve (Or (Not (Var "a")) (And (Not (Var "b")) (Var "c")))
--Just [("c",True),("b",False),("a",True)]

--satSolve (Not (Or (Not (Var "a")) (And (Not (Var "b")) (Var "c"))))
--Just [("c",True),("b",True),("a",True)]

----------------------------------------------helper for constructor---------------------------------------------------------------------
--this function is similar to one in CryptConfig Example 
--the purpose of the function is to return the value of the variable  string from from a tupple (String,Bool) e.g ("a", True) returns True
findValue :: [(String,Bool)] -> String -> Bool                        
findValue env c = case lookup c env of
                Nothing -> error ("undefined variable: " ++ (show c))
                Just nBoolean  -> nBoolean

--findValue [("x",False)] "x"
----------------------------------------------helper for satSolve---------------------------------------------------------------------------
--this function will return the string in the expression
findString :: BExp -> [String]
findString (BConst a) = []
findString (And a b) = nub((findString a) ++ (findString b))
findString (Var a) = [a]
findString (Or a b) = (findString a) ++ (findString b)
findString (Not a) = (findString a) 

--findString (Or (Var "a") (Var "b"))
--["a","b"]
--findString (Or (Not (Var "a")) (And (Not (Var "b")) (Var "c")))
----------------------------------------------------------------------------------------------------------------------------------------------

