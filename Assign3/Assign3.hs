{-# LANGUAGE DatatypeContexts #-}

import Data.Ratio
import Data.List
import Data.String
import Sequences
import Series 

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------Question 1--------------------------------------------------------------------------------------------------------------------


cosPowSeD :: PowSe Double
cosPowSeD = let (PowSe xs) = integrate (-sinPowSeD) + 1
            in PowSe (1:xs)

--cosPowSeD 
--1.0-0.5*x^2+4.1666666666666664e-2*x^4-1.388888888888889e-3*x^6+2.48015873015873e-5*x^8-...
sinPowSeD :: PowSe Double
sinPowSeD = let (PowSe xs) = integrate cosPowSeD
            in PowSe (1:xs)
--sinPowSeD 
--1.0+1.0*x-0.16666666666666666*x^3+8.333333333333333e-3*x^5-1.984126984126984e-4*x^7+2.7557319223985893e-6*x^9...



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------Question 2--------------------------------------------------------------------------------------------------------------------

mySine :: (Ord a, Fractional a) => a -> a
mySine t   |(abs t) < 0.000000001 = t
		   |otherwise = (3*(q)) - (4*(q^3))
		     			where q = mySine (t/3)

-- mySine 45
--0.8509035245341463

-- mySine 90
--0.8939966636005514

--mySine 0
--0.0
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------Question 3--------------------------------------------------------------------------------------------------------------------

sub :: Num t => [t] -> [t] -> [t]
sub [] _ = []
sub _ [] = []
sub (l:ls) (x:xs) = (l - x):(sub ls xs)

--sub [2,2,3] [2,5,12]
--[0,-3,-9]

--sub [6,7,8] [1,2,3,4]
--[5,5,5]

-- sub [1..] [14..25]
--[-13,-13,-13,-13,-13,-13,-13,-13,-13,-13,-13,-13]
scaleList :: Num t => t -> [t] -> [t]
scaleList a lst = [a*x|x<-lst]
--scaleList (2) [1..10]
--[2,4,6,8,10,12,14,16,18,20]

--scaleList (1/2) [2,5,12]
--[1.0,2.5,6.0]


subScale :: Fractional a => [a] -> [a] -> [a]
subScale lst1 lst2 = drop 1 (sub lst2 lst) 
					where lst = scaleList (head lst2) (scaleList (1/(head lst1)) lst1)

--subScale [0,-5,-5][-8,-4,-12]
--[-Infinity,-Infinity]

--subScale [10,-5,-5] [-8,-4,-12]
--[-8.0,-16.0]



--nonZeroFirst lst = getFirstNonZero lst lst 0

nonZeroFirst :: (Num a, Eq a) => [[a]] -> [[a]]
nonZeroFirst lst = getFirstNonZero lst lst 1

getFirstNonZero :: (Num a, Eq a) => [[a]] -> [[a]] -> Int -> [[a]]
getFirstNonZero [] _ _ = error "no such list starting with non zero"
getFirstNonZero lst lst2 n  = if (head (head lst)) /= 0 then  [head lst] ++ (take (n-1) lst2) ++ (drop n lst2) else getFirstNonZero (tail lst) lst2 (n+1)

-- nonZeroFirst [[0,-5,-5],[0,-4,-12],[4,5,6,7]]
-- [[4,5,6,7],[0,-5,-5],[0,-4,-12]]

-- nonZeroFirst [[0,-5,-5],[0,-4,-12],[0,5,6,7]]
-- *** Exception: no such list starting with non zero

triangulate :: (Fractional a, Eq a) => [[a]] -> [[a]]
triangulate  lst = if (length lst) == 0 then [] else [head lst'] ++ triangulate (getTriangulate (head lst') (tail lst'))
								where lst' = (nonZeroFirst lst)


getTriangulate :: Fractional a => [a] -> [[a]] -> [[a]]
getTriangulate headLst lst = [ (subScale headLst lists)| lists <- lst]

--triangulate [[2,3,3,8],[2,3,-2,3],[4,-2,2,4]]
--[[2.0,3.0,3.0,8.0],[-8.0,-4.0,-12.0],[-5.0,-5.0]]


-- triangulate [[6,5,2,4],[1,3,-4,3],[6,-7,7,4]]
--[[6.0,5.0,2.0,4.0],[2.166666666666667,-4.333333333333333,2.3333333333333335],[-18.999999999999993,12.923076923076923]]

dot :: Num a => [a] -> [a] -> a
dot lst lst2 = sum (zipWith (*) lst lst2)

--dot [1,2] [3,4]
--11


solveLine :: Fractional a => [a] -> [a] -> a
solveLine lst valueList = (getLast lst - (dot (tail lst) valueList)) / (head lst)

--solveLine [2,3,3,8] [1,1]

getLast :: [a] -> a			
getLast lst = lst !! (length lst - 1)


solveTriangular :: Fractional t => [[t]] -> [t]
solveTriangular lst = if (length lst == 1) then [(solveLine (head lst) [0])] else [solveLine (head lst) (solveTriangular (tail lst))]++(solveTriangular (tail lst))

--solveTriangular [[2.0,3.0,3.0,8.0],[-8.0,-4.0,-12.0],[-5.0,-5.0]]
--[1.0,1.0,1.0]

solveSystem :: (Fractional t, Eq t) => [[t]] -> [t]
solveSystem lst = solveTriangular (triangulate lst)

--solveSystem [[2,3,3,8],[2,3,-2,3],[4,-2,2,4]]
--[1.0,1.0,1.0]

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------Question 4--------------------------------------------------------------------------------------------------------------------

data Exp = RExp Rational  |
           Var String     |
           Sum Exp Exp    |
           Prod Exp Exp   |
           D Exp String   |
           Sin Exp        |
           Exp Exp        |
           Cos Exp        |
           Ln Exp         |
           Int Exp String |
           Pow Exp Exp  deriving Eq

instance Show Exp where
	show (RExp n)
		| n < 0 && denominator n /= 1 = "(" ++ (show (numerator n)) ++"/"++(show (denominator n))++")"
		| n < 0 			 		  = "("++(show (numerator n))++")" 
		| denominator n == 1 		  = (show (numerator n))
		| otherwise 				  = (show (numerator n)) ++"/"++(show (denominator n))

	show (Var x)       				  = x

	show (Sum u v) 	 				  = showSumContext (show u)++(show v)

	show (Prod (Sum u v) z) 		  = showProdContext ("("++(show (Sum u v))++")")++ (show z)
	show (Prod z (Sum u v)) 		  = showProdContext (show z)++"("++(show (Sum u v))++")"
	show (Prod u v)   				  = showProdContext (show u)++(show v)

	show (Pow (Prod u v) (Sum x y))   = showPowContextLeft (show (Prod u v))++"("++(show (Sum x y))++")"
	show (Pow z (Sum u v))  		  = (show z)++showPowContextRight(show (Sum u v))
	show (Pow (Sum u v) z)   		  = showPowContextLeft (show (Sum u v))++(show z)
	show (Pow (Prod u v) z)  		  = showPowContextLeft (show (Prod u v))++(show z)
	show (Pow (Pow u v) z)   		  = showPowContextLeft (showPowContext (show u)++(show v))++(show z)
	show (Pow z (Pow u v))   		  = showPowContext (show z)++(showPowContext (show u)++(show v))
	show (Pow (RExp u) v) 
		| u < 0 && denominator u /= 1 = showPowContextLeft ((show (numerator u)) ++"/"++(show (denominator u)))++ (show v)
		| u < 0 			 		  = showPowContextLeft (show (numerator u))++(show v)
	 	| denominator u == 1 		  = showPowContext (show (numerator u)) ++ (show v)
	 	| otherwise			  		  = showPowContextLeft ((show (numerator u)) ++"/"++(show (denominator u)))++(show v)
	show (Pow u v) 		 		  	  = showPowContext (show u)++(show v) 
	show (D u v)					  = "D("++show (u)++","++v++")"
	show (Sin u)					  = "Sin("++show u++")"
	show (Cos u)					  = "Cos("++show u++")"
	show (Ln u)					  	  = "Ln("++show u++")"
	show (Int u v)					  = "Int("++show u++")"
	show (Exp u)					  = "e^("++show u++")"

addParens :: [Char] -> [Char]
addParens a 			= "("++a++")"

showSumContext :: [Char] -> [Char]
showSumContext a 		= ""++a++"+"

showProdContext :: [Char] -> [Char]
showProdContext a 		= ""++a++"*"

showPowContext :: [Char] -> [Char]
showPowContext a 		= ""++a++"^"

showPowContextRight :: [Char] -> [Char]
showPowContextRight a	= "^("++a++")"


showPowContextLeft :: [Char] -> [Char]
showPowContextLeft a 	= "("++a++")^"

-------------------------------------------- Test cases -------------------------------------------- 

--(Sum (RExp 2) (Sum (RExp 3) (RExp 4)))
--  2+3+4

--(Sum (Sum (RExp 2) (RExp 3)) (RExp 4))
--2+3+4

--(Sum (RExp 2) (Prod (RExp 3) (RExp 4)))
--2+3*4


--(Prod (Sum (RExp 2) (RExp 3)) (RExp 4))
--  (2+3)*4

--(Prod (RExp (2%3)) (RExp 3))
--2/3*3

--(Pow (RExp (2%3)) (RExp 3))
--(2/3)^3

--(Pow (Sum (RExp 1) (RExp 3)) (RExp 2))
--(1+3)^2

--(Pow (Prod (RExp 4) (RExp 3)) (RExp 2))
--(4*3)^2

--(Pow (Prod (RExp 4) (RExp 3)) (Sum (RExp 2) (RExp 4)))
--(4*3)^(2+4)

--(Pow (RExp (-1)) (RExp 2))
--(-1)^2

--(Pow (Pow (RExp 2) (RExp 3)) (RExp 2))
--(2^3)^2

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------Question 5--------------------------------------------------------------------------------------------------------------------

-------------------------------------------- from class notes -------------------------------------------- 

data MPoly = Const Rational | ProdPlus MPoly Kernel MPoly deriving (Show,Eq)
data Kernel = KVar String deriving Eq
data Eqn = Eqn Exp Exp

instance Show Kernel where
  show (KVar s) = s
instance Ord Kernel where
  compare (KVar x) (KVar y) = compare x y
 

rationalEval :: Exp -> Rational
rationalEval (RExp n) = n
rationalEval (Var x) = error "Variable encountered in rational expression"
rationalEval (Sum u v) = (rationalEval u) + (rationalEval v)
rationalEval (Prod u v) = (rationalEval u) * (rationalEval v)
rationalEval (Pow u v) = let nv = (numerator (rationalEval v))
                   in if nv >= 0 then
                          toRational ((fromRational (rationalEval u)) ^ nv)
                       else error "Fraction encountered in integer expression"
					   
fromExp :: Exp -> MPoly
fromExp (RExp n)   = Const n
fromExp (Var x)    = fromVar x
fromExp (Sum u v)  = (fromExp u) + (fromExp v)
fromExp (Prod u v) = (fromExp u) * (fromExp v)
fromExp (Pow u v)  = let n = numerator (rationalEval v)
                     in if n >=0 then (fromExp u) ^ n
                        else error "Fractional polynomial encountered"
 
fromVar :: String -> MPoly
fromVar x = (ProdPlus (Const 1) (KVar x) (Const 0))

fromConst :: Rational -> MPoly
fromConst a = Const a

scale :: Rational -> MPoly -> MPoly
scale 0 p                  = Const 0
scale a (Const b)          = Const (a*b)
scale a (ProdPlus p1 x p2) = ProdPlus (scale a p1) x (scale a p2)

mulVar :: Kernel -> MPoly -> MPoly -- multiply by x
mulVar x (Const 0)   = (Const 0)
mulVar x p@(Const a) = (ProdPlus p x (Const 0))
mulVar y p@(ProdPlus p1 x p2)
  | x < y            = (ProdPlus (mulVar y p1) x (mulVar y p2))
  | x > y            = (ProdPlus p y (Const 0))
  | otherwise        = (ProdPlus p x (Const 0))

instance Num MPoly where
  (Const a) + (Const b) = Const (a+b)
  (ProdPlus p1 x p2) + (Const a) = ProdPlus p1 x ((Const a) + p2)
  (Const a) + (ProdPlus p1 x p2) = ProdPlus p1 x ((Const a) + p2)
  p@(ProdPlus p1 x p2) + p'@(ProdPlus p1' y p2') 
     | x < y     = ProdPlus p1 x (p2+p')
     | x > y     = ProdPlus p1' y (p+p2')
     | otherwise = normalPoly (p1 + p1') x (p2+p2')

  negate p = scale (-1) p

  (Const a) * p          = scale a p
  (ProdPlus p1 x p2) * p = (p1 * (x `mulVar` p)) + p2*p

  abs _ = error "abs not supported for type MPoly"
  signum _ = error "signum not supported for type MPoly"

  fromInteger  = fromConst . fromInteger
  
normalPoly :: MPoly -> Kernel -> MPoly -> MPoly
normalPoly (Const 0) x p2 = p2
normalPoly p1 x p2        = ProdPlus p1 x p2

value :: [(String, Rational)] -> String-> Rational                        
value env c = case lookup c env of
                Nothing -> toRational (0)
                Just n  -> n

--------------------------------------------  class notes end -------------------------------------------- 


--------------------------------------------  helper functions -------------------------------------------- 

check :: MPoly -> MPoly -> [([Char], Rational)]
check (ProdPlus (Const a) (KVar u) (Const b)) (Const s) = ("equalsTO",(s-b)):[(u,a)]
check (ProdPlus (Const a) (KVar u) (Const b)) (ProdPlus (Const c) (KVar v) (Const d))
	| u == v 	= ("equalsTO",(d-b)):[(v,(a-c))]
	| otherwise = ("equalsTO",(d-b)):((u,a):[(v,b)])
check (ProdPlus (Const a) (KVar u) f) v = [(u,a)] ++ check f v

--check (ProdPlus (Const 5) (KVar "v") (Const 3)) (ProdPlus (Const 2) (KVar "v") (Const 5))
--[("equalsTO",2 % 1),("v",3 % 1)]

examList :: [Eqn] -> [[([Char], Rational)]]
examList exl = [exam x|x<-exl]

exam :: Eqn -> [([Char], Rational)]
exam (Eqn u v )= check (fromExp u) (fromExp v)

--examList [(Eqn (Var "y") (RExp 5)),(Eqn (Sum (Var "x") (Var "y")) (RExp 2))]
--[[("equalsTO",5 % 1),("y",1 % 1)],[("x",1 % 1),("equalsTO",2 % 1),("y",1 % 1)]]

getVariables :: Eq a => [[(a, t1)]] -> [a]
getVariables lst = nub [(getVars tups)|tupList <-lst,tups<-tupList]

getVars :: (t, t1) -> t
getVars tup@(x,y) = x

getTheList :: [[(String, Rational)]] -> [[(String, Rational)]]
getTheList lstOfList = [(addInList tupleList (getVariables lstOfList))| tupleList<-lstOfList] 

checkVar :: [(String,Rational)] -> String -> Rational                        
checkVar tupList var = case lookup var tupList of
                Nothing -> 0
                Just n  -> n

addInList:: [(String, Rational)] -> [String] -> [(String, Rational)]
addInList lst varList = [(var,checkVar lst var)| var<-varList] 

putAtEnd :: [([Char], t)] -> [([Char], t)]
putAtEnd lst = atEnd lst lst 1
atEnd [] _ _ = []
atEnd lst lst2 n = if (checkThis (head lst) "equalsTO") then (take (n-1) lst2)++(drop n lst2)++[head lst] else atEnd (tail lst) (lst2) (n+1)

checkThis :: Eq a => (a, t) -> a -> Bool
checkThis (x,y) c |x == c = True
				  | otherwise = False

--[(Eqn (Var "y") (RExp 5)),(Eqn (Sum (Var "x") (Var "y")) (RExp 2))]

oderList :: Ord a => [[a]] -> [[a]]
oderList lst = [(sortBy compare l)|l<-lst]

getsCoefficient :: [[(t1, t)]] -> [[t]]
getsCoefficient lst = [getsCoeff tLst| tLst<-lst]

getsCoeff :: [(t1, t)] -> [t]
getsCoeff lst = [getFirst tuple| tuple<-lst]


getFirst :: (t, t1) -> t1
getFirst (x,y) = y

checkLength :: [[a]] -> [[a]]
checkLength lst | (length (head lst)-1) <= (length lst) = lst
			  | otherwise 							  = error "The number of variables and number of equations do't match "

--------------------------------------------  helper functions  for 5th end here -------------------------------------------- 
system :: [Eqn] -> [[Rational]]
system lst =  checkLength (nonZeroFirst (getsCoefficient([putAtEnd m|m<-(oderList (getTheList ((examList lst))))])))

--system [(Eqn (Var "y") (RExp 5)),(Eqn (Sum (Var "x") (Var "y")) (RExp 2))]
--[[1 % 1,1 % 1,2 % 1],[0 % 1,1 % 1,5 % 1]]

-- system [(Eqn (Sum (Prod (RExp (-2)) (Var "z")) (Sum (Prod (RExp 3) (Var "y")) (Prod (RExp 2) (Var "x")))) (RExp 3)),(Eqn (Sum (Prod (RExp (-2)) (Var "y")) (Sum (Prod (RExp 4) (Var "x")) (Prod (RExp 2) (Var "z")))) (RExp 4))]
-- *** Exception: The number of variables and number of equations do't match

--system [(Eqn (Sum (Prod (RExp 2) (Var "x")) (Sum (Prod (RExp 3) (Var "z")) (Prod (RExp 3) (Var "y")))) (RExp 8)),(Eqn (Sum (Prod (RExp (-2)) (Var "z")) (Sum (Prod (RExp 3) (Var "y")) (Prod (RExp 2) (Var "x")))) (RExp 3)),(Eqn (Sum (Prod (RExp (-2)) (Var "y")) (Sum (Prod (RExp 4) (Var "x")) (Prod (RExp 2) (Var "z")))) (RExp 4))]
--[[2 % 1,3 % 1,3 % 1,8 % 1],[2 % 1,3 % 1,(-2) % 1,3 % 1],[4 % 1,(-2) % 1,2 % 1,4 % 1]]

--system [(Eqn (Sum (Prod (RExp 2) (Var "x")) (Sum (Prod (RExp 3) (Var "z")) (Prod (RExp 3) (Var "y")))) (RExp 8)),(Eqn (Sum (Prod (RExp (-2)) (Var "z")) (Sum (Prod (RExp 3) (Var "y")) (Prod (RExp 2) (Var "x")))) (RExp 3)),(Eqn (Sum (Prod (RExp (-2)) (Var "y")) (Sum (Prod (RExp 4) (Var "x")) (Prod (RExp 2) (Var "z")))) (RExp 4))]

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------Question 6 a and b and Graduate section 2 a------------------------------------------------------------------------------------------------------------------

visitOnce :: (Exp -> Exp) -> Exp -> Exp
visitOnce f e@(RExp n) = e
visitOnce f e@(Var x)  = e
visitOnce f (Sum u v)  = f (Sum (visitOnce f u) (visitOnce  f v))
visitOnce f (Prod u v) = f (Prod (visitOnce f u) (visitOnce f v))
visitOnce f (Pow u v)  = f (Pow (visitOnce f u) (visitOnce f v))


visitUntilUnchanged :: (Exp -> Exp) -> Exp -> Exp
visitUntilUnchanged f tr = let new = visitOnce f tr
                           in if new == tr
                              then tr
                              else visitUntilUnchanged f new

simplify3 :: Exp -> Exp
simplify3 (Int (RExp w) z)   = Prod (RExp w) (Var z)
simplify3 (Int (Var x) z)
	| x == z 				 = Prod (RExp (1%2)) (Pow (Var x) (RExp 2))

simplify3 (Int (Pow (Var u) (RExp v)) z)
	| u == z 				 = simplify3(Prod (RExp (1/v)) (Pow (Var u) (RExp (v+1))))

simplify3 (Int (Sin u) z)  	 = simplify3 (Prod (RExp 1) (Cos u))
simplify3 (Int (Cos u) z) 	 = simplify3 (Prod (RExp (-1)) (Sin u))
simplify3 (Int (Sum u v) z)  = simplify3 (Sum (simplify3 (Int u z)) (simplify3 (Int v z)))
simplify3 (Int (Prod u v) z) 
	| simplify3 (D u z) == v = Prod (RExp (1%2)) (Pow u (RExp 2))

simplify3 (D (RExp w) z)  	 = RExp 0
simplify3 (D (Var v) q)
  | v == q 	  			  	 = RExp 1 
  | otherwise 			     = RExp 0	
simplify3 (D (Sum u v) w)    = visitUntilUnchanged simplify3 (Sum (simplify3 (D u w)) (simplify3 (D v w)))



simplify3 (Prod (RExp u) (D (Pow (Var v) (RExp n))  x))	= simplify3 (Prod (simplify3 (Prod (RExp u) (RExp n))) (simplify3 ((Pow (Var v) (RExp (n-1))))))
simplify3 (D (Prod (RExp u) (Pow (Var v) (RExp n))) x) 	= visitUntilUnchanged simplify3 (Prod (simplify3(Prod (RExp u) (RExp n))) (simplify3 ((Pow (Var v) (RExp (n-1))))))

simplify3 (D (Prod (RExp u) q) x) 			= simplify3 (Prod (RExp u) (simplify3 (D q x)))
simplify3 (D (Prod (Var u) (Pow (RExp c) (Var v)))  z)
	= simplify3 (simplify3(Sum (simplify3 (Prod (Var u) (simplify3 (D (Pow (RExp c) (Var v))  z)))) (simplify3 (Prod (Pow (RExp c) (Var v)) (simplify3 (D (Var u) z))))))
simplify3 (D (Prod u v) w) 		  			= visitUntilUnchanged simplify3 (simplify3(Sum (simplify3 (Prod u (simplify3 (D v w)))) (simplify3 (simplify3 (Prod v (simplify3 (D u w)))))))
simplify3 (D (Pow u (RExp n)) z)  			
	| n == 0 = RExp 0
	|otherwise = simplify3(Prod (RExp n) (simplify3 (Prod (simplify3 (Pow u (RExp (n-1)))) (simplify3 (D u z)))))
simplify3 (D (Pow (RExp 0) v) z) 	   		= RExp 0
simplify3 (D (Pow (RExp u) v) z) 	   		= D (Pow (RExp u) v) z
simplify3 (D (Sin u) z)						= simplify3(Prod( simplify3(D u z)) (Cos u))
simplify3 (D (Cos x) z)						= simplify3(Prod (RExp (-1)) (Prod (simplify3(D x z)) (Sin x)))
simplify3 (D (Ln u) z)
	| u == (RExp 1) = (RExp 0)
	| u == (Var z) 	= (Pow u (RExp (-1)))
simplify3 (D (Exp u) z) 					
	| simplify3 (D u z) /= RExp 0 = simplify3 (Prod (simplify3 (D u z)) (Exp u))
	| otherwise = RExp 0

simplify3 (Sum (RExp n) (RExp m))           = RExp (n+m)
simplify3 (Sum (RExp n) v)                  = (Sum v (RExp n))
simplify3 (Sum u (RExp 0))                  = u
simplify3 (Sum (Sum u (RExp n)) (RExp m))   = Sum u (RExp (n+m))
simplify3 (Sum (Sum u (RExp n)) v)          = Sum (Sum u v) (RExp n)
simplify3 (Sum u (Sum v w))                 = Sum (Sum u v) w
simplify3 (Sum u v)
  | u == v                              	= Prod (RExp 2) u
simplify3 (Sum (Prod (RExp n) u) v)
  | u == v                              	= Prod (RExp (n+1)) u
simplify3 (Sum u (Prod (RExp n) v))
  | u == v                              	= Prod (RExp (n+1)) u
simplify3 (Sum (Prod (RExp n) u) (Prod (RExp m) v))
  | u == v                              	= Prod (RExp (n+m)) u
simplify3 (Prod (RExp n) (RExp m))          = RExp (n*m)
simplify3 (Prod (RExp 0) v)                 = RExp 0
simplify3 (Prod (RExp 1) v)                 = v
simplify3 (Prod u (RExp n))                 = Prod (RExp n) u
simplify3 (Prod (RExp n) (Prod (RExp m) v)) = Prod (RExp (n*m)) v
simplify3 (Prod (Var u) (Var v))
	| u == v = Pow (Var u) (RExp 2)
simplify3 (Prod u (Prod (RExp n) v))        = Prod (RExp n) (Prod u v)
simplify3 (Prod (Prod u v) w)               = Prod u (Prod v w)
simplify3 (Prod (RExp n) (Sum u v))         = Sum (Prod (RExp n) u) (Prod (RExp n) v)
simplify3 (Prod u (D x z))					= Prod u (simplify3 (D x z))
simplify3 (Pow u (RExp 0))                  = RExp 1
simplify3 (Pow u (RExp 1))                  = u
simplify3 (Pow u (RExp n))                  = Pow u (RExp (n))
simplify3 u 								= u

--simplify3 (D (RExp 2) "x")
--0
--simplify3 (D (Var "x") "x")
--1

--simplify3 (D (Var "y") "x")
--0

--simplify3 (D (Sum (RExp 2) (Var "x")) "x")
--1

-- simplify3 (D (Prod (Var "x") (Var "x")) "x")
--2*x

--simplify3 (D (Prod (RExp 5) (Var "x")) "x")
--5

--simplify3 (D (Prod (Var "x") (Var "x")) "x")
--2*x

--simplify3 (D (Prod (Var "x") (Var "x")) "x")
--2*x

--simplify3 (D (Prod (Var "x") (Sum (Var "x") (RExp 3))) "x")
--2*x+3

--simplify3 (D (Pow (Var "x") (RExp 2))  "x")
--2*x

--simplify3 (D (Prod (RExp 5) (Pow (RExp 2) (Var "x")))  "x")
--5*D(2^x,x)

--simplify3 (D (Prod (Var "x") (Pow (RExp 2) (Var "x")))  "x")
--x*D(2^x,x)+1*2^x


--simplify3 (Prod (RExp 5) (D (Pow (Var "x") (RExp 2))  "x"))
--10*x

------------------------------------------graduate 2 a------------------------------------------

--simplify3 (D (Sin(Var "u")) "u")
--Cos(u)

--simplify3 (D (Cos(Var "u")) "u")
--(-1)*Sin(u)

-- simplify3 (D (Ln(Var "u")) "u")
--u^(-1)

--simplify3 (Int (Var "s") "s")
--1/2*s^2

--simplify3 (Int (Sum (RExp 5) (RExp 6)) "z")
--11*z

--simplify3 (Int (Var "x") "1")
--Int(x)

 --simplify3 (D (Exp (Var "z")) "z")
--e^(z)


-------------------------------------------- Test cases -------------------------------------------- 

--(D (Pow (Var "x") (RExp 2)) "x")
--D(x^2,x)

--(Prod (RExp 5) (D (Pow (Var "x") (RExp 2)) "x"))
--5*D(x^2,x)

--simplify3 (D (RExp 2) "x")
--0

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------Question graduate 1--------------------------------------------------------------------------------------------------------------------
myExp :: Double -> Double
myExp x = limitD(seqFromPowSe expPowSeD x)

-- myExp 1
--2.718281828459045

-- myExp 0
-- 1.0


lnPlusPowSeD :: PowSe Double
lnPlusPowSeD = shift (integrate (1/(1+var)))

--lnPlusPowSeD 
--1.0*x-0.5*x^2+0.3333333333333333*x^3-0.25*x^4+0.2*x^5-0.16666666666666666*x^6+0.14285714285714285*x^7-0.125*x^8+0.1111111111111111*x^9-...

lnPlusPowSeR :: PowSe Rational
lnPlusPowSeR = shift (integrate (1/(1+var)))
--lnPlusPowSeR
--1 % 1*x-1 % 2*x^2+1 % 3*x^3-1 % 4*x^4+1 % 5*x^5-1 % 6*x^6+1 % 7*x^7-1 % 8*x^8+1 % 9*x^9-...


reduce ::(Double)->(Double,Integer)
reduce x = reduceThis x 0


--reduce 4000
--(0.4,4)

--reduce (-4000)
--(-0.4,4)


reduceThis :: (Ord t, Num t1, Fractional t) => t -> t1 -> (t, t1)
reduceThis x n | abs(x') < 1 = ( x', n+1)
			   | otherwise = reduceThis x' (n+1)
			   		where x' = (x/10)

improveSqrtGuess1 :: Double -> Double -> Double
improveSqrtGuess1 a g = g + 2*((a - (myExp g))/(a + (myExp g)))

--improveSqrtGuess1 40 1
--2.7454689924679396
 
myLn a = Seq (iterate (improveSqrtGuess1 a) 1)
--

--myLn 1
--1.0,7.576568547998053e-2,3.622323167375152e-5,3.953753286139311e-15,-4.304960251124467e-17,-4.304960251124467e-17,-4.304960251124467e-17,...

mySqrt' (x) = myExp (0.5*(limitD (myLn x)))
-- mySqrt' 100
--9.999999999999996

--mySqrt' 4
----2.0

--mySqrt' 25
--5.0



--[(Eqn (Var "y") (RExp 5)),(Eqn (Sum (Var "x") (Var "y")) (RExp 2))]
--[(Eqn (Var "y") (RExp 5)), (Eqn (Sum (Var "x") (Var "y")) (RExp 2))]


--gets only the variables without the equals to
getOnlyVar ls = nub ([var|var <-ls, var /= "equalsTO"])


