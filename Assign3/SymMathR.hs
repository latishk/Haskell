{-# LANGUAGE DatatypeContexts #-}

import Data.Ratio
import Data.List
import Data.String
import Sequences
import Series 


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------Question 2--------------------------------------------------------------------------------------------------------------------


mySine t   |(abs t) < 0.000000001 = t
		   |otherwise = (3*(q)) - (4*(q^3))
		     			where q = mySine (t/3)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------Question 3--------------------------------------------------------------------------------------------------------------------
sub [] _ = []
sub _ [] = []
sub (l:ls) (x:xs) = (l - x):(sub ls xs)

scaleList a lst = [a*x|x<-lst]

subScale lst1 lst2 = drop 1 (sub lst2 lst) 
					where lst = scaleList (head lst2) (scaleList (1/(head lst1)) lst1)

--nonZeroFirst lst = getFirstNonZero lst lst 0
nonZeroFirst lst = getFirstNonZero lst lst 1
getFirstNonZero [] _ _ = error "no such list starting with non zero"
getFirstNonZero lst lst2 n  = if (head (head lst)) /= 0 then  [head lst] ++ (take (n-1) lst2) ++ (drop n lst2) else getFirstNonZero (tail lst) lst2 (n+1)

triangulate  lst = if (length lst) == 0 then [] else [head lst'] ++ triangulate (getTriangulate (head lst') (tail lst'))
								where lst' = (nonZeroFirst lst)

getTriangulate headLst lst = [ (subScale headLst lists)| lists <- lst]

dot lst lst2 = sum (zipWith (*) lst lst2)

solveLine lst valueList = (getLast lst - (dot (tail lst) valueList)) / (head lst)
			
getLast lst = lst !! (length lst - 1)

solveTriangular lst = if (length lst == 1) then [(solveLine (head lst) [0])] else [solveLine (head lst) (solveTriangular (tail lst))]++(solveTriangular (tail lst))
			
solveSystem lst = solveTriangular (triangulate lst)


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------Question 4--------------------------------------------------------------------------------------------------------------------

data Exp = RExp Rational  |
           Var String     |
           Sum Exp Exp    |
           Prod Exp Exp   |
           D Exp String   |
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

addParens a 			= "("++a++")"
showSumContext a 		= ""++a++"+"
showProdContext a 		= ""++a++"*"
showPowContext a 		= ""++a++"^"
showPowContextRight a	= "^("++a++")"
showPowContextLeft a 	= "("++a++")^"



visitOnce :: (Exp -> Exp) -> Exp -> Exp
visitOnce f e@(RExp n) = e
visitOnce f e@(Var x)  = e
visitOnce f (Sum u v)  = f (Sum (visitOnce f u) (visitOnce  f v))
visitOnce f (Prod u v) = f (Prod (visitOnce f u) (visitOnce f v))
visitOnce f (Pow u v)  = f (Pow (visitOnce f u) (visitOnce f v))

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------Question 5--------------------------------------------------------------------------------------------------------------------








------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------Question 6--------------------------------------------------------------------------------------------------------------------



--simplify3 (D (Sum (RExp u) (Var v)) z)  = simplify3 (Sum (simplify3 (D (RExp u) z)) (simplify3 (D (Var v) z)))
--simplify3 (D (Su
--simplify3 (D (Prod (RExp u) (Var v)) z) = simplify3(Prod (RExp u) (simplify3 (D (Var v) z)))

visitUntilUnchanged :: (Exp -> Exp) -> Exp -> Exp
visitUntilUnchanged f tr = let new = visitOnce f tr
                           in if new == tr
                              then tr
                              else visitUntilUnchanged f new

simplify3 :: Exp -> Exp
simplify3 (D (RExp w) z)  = RExp 0
simplify3 (D (Var v) q)
  | v == q 	  			  = RExp 1 
  | otherwise 			  = RExp 0	
simplify3 (D (Sum u v) w) = visitUntilUnchanged simplify3 (Sum (simplify3 (D u w)) (simplify3 (D v w)))

simplify3 (Prod (RExp u) (D (Pow (Var v) (RExp n))  x))	= simplify3 (Prod (simplify3 (Prod (RExp u) (RExp n))) (simplify3 ((Pow (Var v) (RExp (n-1))))))
simplify3 (D (Prod (RExp u) (Pow (Var v) (RExp n))) x) 	= visitUntilUnchanged simplify3 (Prod (Prod (RExp u) (RExp n)) (simplify3 ((Pow (Var v) (RExp (n-1))))))

simplify3 (D (Prod (RExp u) q) x) 			= Prod (RExp u) (simplify3 (D q x))
simplify3 (D (Prod u v) w) 		  			= simplify3 (Sum (simplify3 (Prod u (simplify3 (D v w)))) (simplify3 (Prod v (simplify3 (D u w)))))
simplify3 (D (Pow u (RExp n)) z)  			= simplify3(Prod (RExp n) (simplify3 (Prod (simplify3 (Pow u (RExp (n-1)))) (simplify3 (D u z)))))
simplify3 (D (Pow (RExp u) v) z) 	   		= D (Pow (RExp u) v) z
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

--handle the cases where it prod x x and again prod with x, ie prod should give u x^3



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------Question 5--------------------------------------------------------------------------------------------------------------------
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


check :: MPoly -> MPoly -> [([Char], Rational)]
check (ProdPlus (Const a) (KVar u) (Const b)) (Const s) = ("equalsTO",(s-b)):[(u,a)]
check (ProdPlus (Const a) (KVar u) (Const b)) (ProdPlus (Const c) (KVar v) (Const d))
	| u == v 	= ("equalsTO",(d-b)):[(v,(a-c))]
	| otherwise = ("equalsTO",(d-b)):((u,a):[(v,b)])
check (ProdPlus (Const a) (KVar u) f) v = [(u,a)] ++ check f v


examList :: [Eqn] -> [[([Char], Rational)]]
examList exl = [exam x|x<-exl]

--[(Eqn (Var "y") (RExp 5)),(Eqn (Sum (Var "x") (Var "y")) (RExp 2))]
--[(Eqn (Var "y") (RExp 5)), (Eqn (Sum (Var "x") (Var "y")) (RExp 2))]
exam (Eqn u v )= check (fromExp u) (fromExp v)
--gets the variables including the equalsTo
--getVariables :: [[(t, t1)]] -> [t]
getVariables lst = nub [(getVars tups)|tupList <-lst,tups<-tupList]
getVars :: (t, t1) -> t
getVars tup@(x,y) = x


--gets only the variables without the equals to
getOnlyVar ls = nub ([var|var <-ls, var /= "equalsTO"])


getTheList lstOfList = [(addInList tupleList (getVariables lstOfList))| tupleList<-lstOfList] 

----addInList lst@[(u,v)] varList = 


checkVar :: [(String,Rational)] -> String -> Rational                        
checkVar tupList var = case lookup var tupList of
                Nothing -> 0
                Just n  -> n

addInList:: [(String, Rational)] -> [String] -> [(String, Rational)]
addInList lst varList = [(var,checkVar lst var)| var<-varList] 


putAtEnd lst = atEnd lst lst 1
atEnd [] _ _ = []
atEnd lst lst2 n = if (checkThis (head lst) "equalsTO") then (take (n-1) lst2)++(drop n lst2)++[head lst] else atEnd (tail lst) (lst2) (n+1)


checkThis (x,y) c |x == c = True
				  | otherwise = False

--[(Eqn (Var "y") (RExp 5)),(Eqn (Sum (Var "x") (Var "y")) (RExp 2))]

system lst =  checkLength (nonZeroFirst (getsCoefficient([putAtEnd m|m<-(oderList (getTheList ((examList lst))))])))

oderList lst = [(sortBy compare l)|l<-lst]


getsCoefficient lst = [getsCoeff tLst| tLst<-lst]


getsCoeff lst = [getFirst tuple| tuple<-lst]

getFirst (x,y) = y

checkLength lst | (length (head lst)-1) <= (length lst) = lst
			  | otherwise 							  = error "The number of variables and number of equations do't match "

--system [(Eqn (Sum (Prod (RExp 2) (Var "x")) (Sum (Prod (RExp 3) (Var "z")) (Prod (RExp 3) (Var "y")))) (RExp 8)),(Eqn (Sum (Prod (RExp (-2)) (Var "z")) (Sum (Prod (RExp 3) (Var "y")) (Prod (RExp 2) (Var "x")))) (RExp 3)),(Eqn (Sum (Prod (RExp (-2)) (Var "y")) (Sum (Prod (RExp 4) (Var "x")) (Prod (RExp 2) (Var "z")))) (RExp 4))]

--system [(Eqn (Var "y") (RExp 5)),(Eqn (Sum (Var "x") (RExp 1)) (RExp 2))]


--getsCoefficient  	
--putAtEnd tupleList  = if  


--addInList lst varList = [/(contains v varInList)  , varInList = getVariables [lst]]
--exam (Sum (Prod (RExp (-2)) (Var "z"))(Sum (Prod (RExp 3) (Var "y"))(Prod (RExp 2) (Var "x")))) (Sum (Prod (RExp (-2)) (Var "x"))(Sum (Prod (RExp 3) (Var "z"))(Prod (RExp 2) (Var "u"))))

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------Question graduate 1 --------------------------------------------------------------------------------------------------------------------

