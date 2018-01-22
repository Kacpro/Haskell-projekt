import Matrix
import Test.QuickCheck



prop_transpose m = transpose (transpose m) == m 

prop_detAfterTranspose m = ((rows m) == (columns m)) ==> det (transpose m) == det m

prop_rowsAndColumns m = rows (transpose m) == columns m 

prop_columnsAndRows m = columns (transpose m) == rows m 

prop_deleteRow m =  (rows m) - 1 == rows (deleteRow m 1)  || (rows m ==0)

prop_deleteColumn m =  (columns m) - 1 == columns (deleteColumn m 1)  || (columns m ==0)

prop_deleteRowAndColumn m =  ((columns m) - 1 == columns (deleteColumn m 1)  || (columns m ==0)) && ((rows m) - 1 == rows (deleteRow m 1)  || (rows m ==0))

prop_addMul m = (mAdd m m) == (mul m 2)

prop_detMul m  a = (rows m) == (columns m) ==> det (mul m a) == a^(rows m) * (det m)

prop_minor m  = (rows m) == (columns m) ==> m == minorMatrix m 1 1 (rows m)

prop_rank m  = rank m <= min (rows m) (columns m)



