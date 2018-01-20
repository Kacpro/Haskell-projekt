module Matrix (Matrix, rows, columns) where



newtype Matrix a = Matrix [[a]] 

rows :: Matrix a -> Int
rows (Matrix m) = length m

columns :: Matrix a -> Int
columns (Matrix m) = length (head m)


instance Show a => Show (Matrix a) where
	show (Matrix m) = "\n" ++ (foldl (++) "" (map (\x -> (( foldr(++) "\n\n" (map (\y -> ((show y) ++ "\t") ) x)))) m))
	

getElement :: Matrix a -> Int -> Int -> a
getElement (Matrix m) y x = (_getElemFromList (_getElemFromList m y) x)


_getElemFromList :: (Num n, Eq n) => [a] -> n -> a
_getElemFromList  (x:xs) 1 = x
_getElemFromList (x:xs) n = _getElemFromList xs (n-1)

_delElemFromList :: (Num n, Eq n) => [a] -> n -> [a]
_delElemFromList (x:xs) 1 = xs
_delElemFromList (x:xs) n = x :_delElemFromList xs (n-1)

deleteRow :: (Num n, Eq n) => (Matrix a) -> n -> (Matrix a)
deleteRow (Matrix m) n = Matrix (_delElemFromList m n)

deleteColumn :: (Num n, Eq n) => (Matrix a) -> n -> (Matrix a)
deleteColumn (Matrix m) n = Matrix (map (\x -> _delElemFromList x n) m)
	
deleteRowAndColumn :: (Num x, Eq x, Num y, Eq y) => (Matrix a) -> x -> y -> (Matrix a)
deleteRowAndColumn m y x = (deleteColumn (deleteRow m y) x)

	
det :: Num a => Matrix a -> a
det m =  if (rows m) == (columns m)
			 then if (rows m) == 1
					 then getElement m 1 1
					 else sum [ (-1)^(i+1) * (getElement m i 1) * (det (deleteRowAndColumn m i 1)) | i <- [1..(rows m)]] 
			  else error "The number of rows doesn't match with the number of columns"

			  
transpose :: (Matrix a) -> (Matrix a)
transpose m = Matrix ([  [ (getElement m i j) | i<-[1..(rows m)] ] | j<- [1..(columns m)] ])


cofactor :: Num a => (Matrix a) -> (Matrix a)
cofactor m =  Matrix (   [  [  ((-1)^(i+j))*det(deleteRowAndColumn m j i) | i<- [1..(columns m)] ] | j <- [1..(rows m)] ]  )

mul :: Num a => (Matrix a) -> a -> (Matrix a)
mul (Matrix m) n = Matrix (map (\x -> map (*n) x) m)

inverse ::(Num a, Fractional a) => (Matrix a) -> (Matrix a)
inverse m = mul (transpose (cofactor m)) (1/(det m))


mMul :: Num a => (Matrix a) -> (Matrix a) -> (Matrix a)
mMul m1 m2 = if ( columns m1) == (rows m2)
					   then Matrix ( [[( sum [ ( (getElement m1 j k)*(getElement m2 k i) ) | k <- [1..(columns m1)] ] ) | i <-[1..(columns m2)] ] | j<- [1..(rows m1)] ] )
					   else error "Can't perform multiplication"
					   
					   
mAdd :: Num a => (Matrix a) -> (Matrix a) -> (Matrix a)
mAdd m1 m2 = Matrix ([[((getElement m1 j i)+(getElement m2 j i)) | i<-[1..(columns m1)]] | j<- [1..(rows m1)]])


minorMatrix :: (Matrix a) -> Int -> Int -> Int -> (Matrix a)
minorMatrix m y x d 
	| y+d-1 < (rows m) = (minorMatrix (deleteRow m (rows m)) y x d)
	| x+d-1 < (columns m) = (minorMatrix (deleteColumn m (columns m)) y x d)
	| y > 1 = (minorMatrix (deleteRow m 1) (y-1) x d)
	| x > 1 = (minorMatrix (deleteColumn m 1) y (x-1) d)
	| (x == 1 && y==1 && d ==(rows m) && d==(columns m))  = m


rank :: (Num a, Eq a)=> (Matrix a) -> Int
rank m = rows (head (filter (\x -> (det x) /=0 )  [ (minorMatrix m i j k) | k<- [(min (columns m) (rows m)), (min (columns m) (rows m)-1)..1], i<- [1..((rows m)-k+1)], j<- [1..((columns m)-k+1)] ]))


solve :: (Num a, Fractional a, Eq a) => (Matrix a) -> (Matrix a) -> (Matrix a)
solve mA mB = if (rows mA)==(columns mA) && (rank mA)==(rows mA) && (columns mB == 1)&& (rows mB == rows mA)
					  then mMul (inverse mA) mB
					  else (Matrix [[]])














			 
