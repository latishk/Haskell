--1
second::[a] -> a
second n = last(take 2 n)
--
--2
singleton::[a]->Bool
singleton n = (1 == (length n))

--3
index :: (Num p, Eq q) => q -> [q] -> Maybe p
index x lst = helper 0 x lst
helper a b [] = Nothing
helper a b (x:xs) = if x == b 
						then Just a
					else 
						helper (a+1) b xs	 


--4
evenSquares' :: Integral b => [b] -> [b]
evenSquares' x = map(^2) (filter (even) x )

--evenSquares' [1..10]

--5
insert :: Ord a => a -> [a] -> [a]
insert b [] = b:[]
insert b (x:xs) = if b <= x 
				then b:(x:xs)
				else x : (insert b xs)
--insert 1 [2..10]
--insert 3 [1,2,4]

insertionSort :: Ord t => [t] -> [t]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
--insertionSort [2,4,5,8,6,8]		

--6
insertionSortH :: Ord t => [t] -> [t]
insertionSortH [] = []
insertionSortH l = foldr insert [head l] (tail l)  
--insertionSortH [2,4,5,8,6,8]

--8 a
data Peano = Zero |S Peano deriving Show

--8 b
add :: Peano -> Peano -> Peano
add Zero Zero= Zero
add Zero z = z
add z Zero = z
add (S z) y = S(add z y)
--add (S Zero) (S (S Zero))

-- 8 c
mult :: Peano -> Peano -> Peano
mult Zero z = Zero
mult (S z) y = add(mult z y) y
 --mult (S (S Zero)) (S (S (S Zero)))

-- 8 d
fact :: Peano -> Peano
fact Zero = (S Zero)
fact (S Zero) = (S Zero)
fact (S z) = mult (S z) z
--fact (S Zero)

--7
helperP :: Eq t => t -> [t] -> [t]
helperP z []=[]
helperP z (x:xs) = if z == x
						then xs
					else x:(helperP z (xs))  

perm :: Eq t => [t] -> [[t]]
perm [] =[[]]
perm x = [z:zs|z<-x, zs<-perm(helperP z x)]
-- perm [2,3,4]


--9a
meaning Zero = (\s z -> z)
meaning (S n) = (\s z -> s (meaning (n) s z))
--meaning Zero id 1

--9b
fromPeano :: Num t => Peano -> t
fromPeano Zero = meaning Zero id 0
fromPeano (S a) = meaning Zero id (fromPeano a) + 1

--fromPeano (S (S Zero))
