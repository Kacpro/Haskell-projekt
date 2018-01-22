import Matrix
import Test.QuickCheck


-- | checks if the matrix changes after double transposure
prop_transpose m = transpose (transpose m) == m 

-- | checks if transposure changes the determinant
prop_detAfterTranspose m = ((rows m) == (columns m)) ==> det (transpose m) == det m

-- | checks if the transposure correctly switches rows and columns
prop_rowsAndColumns m = rows (transpose m) == columns m 

-- | checks if the transposure correctly switches rows and columns
prop_columnsAndRows m = columns (transpose m) == rows m 

-- | checks if deleting a row reduces the number of rows
prop_deleteRow m =  (rows m) - 1 == rows (deleteRow m 1)  || (rows m ==0)

-- | checks if deleting a column reduces the number of columns
prop_deleteColumn m =  (columns m) - 1 == columns (deleteColumn m 1)  || (columns m ==0)

-- | checks if deleting a row and a column reduces the number of rows and columns
prop_deleteRowAndColumn m =  ((columns m) - 1 == columns (deleteColumn m 1)  || (columns m ==0)) && ((rows m) - 1 == rows (deleteRow m 1)  || (rows m ==0))

-- | checks if 1+1 = 2*1
prop_addMul m = (mAdd m m) == (mul m 2)

-- | checks if det( a*m) = a^n * (det m)
prop_detMul m  a = (rows m) == (columns m) ==> det (mul m a) == a^(rows m) * (det m)

-- | checks if maximal minor matrix is the whole matrix
prop_minor m  = (rows m) == (columns m) ==> m == minorMatrix m 1 1 (rows m)

-- | checks if the rank of the matrix is smaller then the size of it
prop_rank m  = rank m <= min (rows m) (columns m)



