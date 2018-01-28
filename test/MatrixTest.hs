module MatrixTest where 

import Matrix
import Test.HUnit

m0 = matrix [[]]
m1 = matrix [[1,2],[3,4]]
m2 = matrix [[1,1,3,2],[4,2,1,3],[1,3,5,2]]
m3 = matrix [[1,4],[5,6],[1,7]]
m4 = matrix [[1]]
m5 = matrix [[1,0,0],[0,1,0],[0,0,1]]


matrixtests = test [ "rowsTest1" ~: "(rows m0)" ~: (0) ~=? (rows m0),
					"rowsTest2" ~: "(rows m1)" ~: (2) ~=? (rows m1),
					"rowsTest3" ~: "(rows m2)" ~: (3) ~=? (rows m2),
					"rowsTest4" ~: "(rows m3)" ~: (3) ~=? (rows m3),
					"rowsTest5" ~: "(rows m4)" ~: (1) ~=? (rows m4),
					
					"columnsTest1" ~: "(columns m0)" ~: (0) ~=? (columns m0),
					"columnsTest2" ~: "(columns m1)" ~: (2) ~=? (columns m1),
					"columnsTest3" ~: "(columns m2)" ~: (4) ~=? (columns m2),
					"columnsTest4" ~: "(columns m3)" ~:(2) ~=? (columns m3),
					"columnsTest5" ~: "(columns m4)" ~: (1) ~=? (columns m4),
					
					"getElementTest1" ~: "(getElement m1 2 1)" ~: (3) ~=? (getElement m1 2 1),
					"getElementTest2" ~: "(getElement m4 1 1)" ~: (1) ~=? (getElement m4 1 1),
					"getElementTest3" ~: "(getElement m3 3 2)" ~: (7) ~=? (getElement m3 3 2),
					
					"deleteRowTest1" ~: "(deleteRow m3 2)" ~: (matrix [[1,4],[1,7]]) ~=? (deleteRow m3 2),
					"deleteRowTest2" ~: "(deleteRow m3 4)" ~: (matrix [[1,4],[5,6],[1,7]]) ~=? (deleteRow m3 4),
					
					"deleteColumnTest1" ~: "(deleteColumn m2 3)" ~: (matrix [[1,1,2],[4,2,3],[1,3,2]]) ~=? (deleteColumn m2 3),
					"deleteColumnTest2" ~: "(deleteColumn m1 1)" ~: (matrix [[2],[4]]) ~=? (deleteColumn m1 1),
					
					"deleteRowAndColumnTest1" ~: "(deleteRowAndColumn m1 1 1)" ~: (matrix [[4]]) ~=? (deleteRowAndColumn m1 1 1),
					"deleteRowAndColumnTest2" ~: "(deleteRowAndColumn m2 2 3)" ~: (matrix [[1,1,2],[1,3,2]]) ~=? (deleteRowAndColumn m2 2 3),
					
					"detTest1" ~: "(det m1)" ~: (-2) ~=? (det m1),
					"detTest2" ~: "(det m4)" ~: (1) ~=? (det m4),
					"detTest3" ~: "(det m5)" ~: (1) ~=? (det m5),
					
					"transposeTest1" ~: "(transpose m4)" ~: (m4) ~=? (transpose m4),
					"transposeTest2" ~: "(transpose m5)" ~: (m5) ~=? (transpose m5),
					"transposeTest3" ~: "(transpose m3)" ~: (matrix [[1,5,1],[4,6,7]]) ~=? (transpose m3),
					
					"cofactorTest1" ~: "(cofactor m4)" ~: (matrix [[4,-3],[-2,1]]) ~=? (cofactor m1),
					"cofactorTest2" ~: "(cofactor m4)" ~: (m5) ~=? (cofactor m5),
					
					"mulTest1" ~: "(mul m4)" ~: (matrix [[5]]) ~=? (mul m4 5),
					"mulTest2" ~: "(mul m4)" ~: (matrix [[2,4],[6,8]]) ~=? (mul m1 2),
					"mulTest3" ~: "(mul m4)" ~: (matrix [[0,0,0,0],[0,0,0,0],[0,0,0,0]]) ~=? (mul m2 0),
					
					"inverseTest1" ~: "(inverse m4)" ~: (m4) ~=? (inverse m4),
					"inverseTest2" ~: "(inverse m1)" ~: (matrix [[-2,1],[1.5, -0.5]]) ~=? (inverse m1),
					"inverseTest3" ~: "(inverse m5)" ~: (m5) ~=? (inverse m5),
					
					"mMulTest1" ~: "(mMul m4 m4)" ~: (m4) ~=? (mMul m4 m4),
					"mMulTest2" ~: "(mMul m3 m1)" ~: (matrix [[13,18],[23,34],[22,30]]) ~=? (mMul m3 m1),
					"mMulTest3" ~: "(mMul m5 m2)" ~: (m2) ~=? (mMul m5 m2),
					
					"mAddTest1" ~: "(mAdd m4 m4)" ~: (matrix [[2]]) ~=? (mAdd m4 m4),
					"mAddTest2" ~: "(mAdd m4 (matrix [[1,1],[2,2],[3,3]]))" ~: (matrix [[2,5],[7,8],[4,10]]) ~=? (mAdd m3 (matrix [[1,1],[2,2],[3,3]])),
					
					"minormatrixTest1" ~: "(minorMatrix m4 1 1 1)" ~: (m4) ~=? (minorMatrix m4 1 1 1),
					"minormatrixTest2" ~: "(minorMatrix m5 1 2 2)" ~: (matrix [[0,0],[1,0]]) ~=? (minorMatrix m5 1 2 2),
					"minormatrixTest3" ~: "(minorMatrix m3 2 1 2)" ~: (matrix [[5,6],[1,7]]) ~=? (minorMatrix m3 2 1 2),
					
					"rankTest1" ~: "(rank m4)" ~: (1) ~=? (rank m4),
					"rankTest2" ~: "(rank m1)" ~: (2) ~=? (rank m1),
					"rankTest3" ~: "(rank m2)" ~: (3) ~=? (rank m2),
					
					"solveTest1" ~: "(solve1)" ~: (matrix [[]]) ~=? (solve (matrix [[1,1],[2,2]]) (matrix [[1],[2]])),
					"solveTest1" ~: "(solve2)" ~: (matrix [[-9],[10],[-2]]) ~=? (solve (matrix [[1,1,1],[1,2,4],[1,3,9]]) (matrix [[-1],[3],[3]])),
					"solveTest1" ~: "(solve3)" ~: (matrix [[]]) ~=? (solve (matrix [[1],[1]]) (matrix [[1],[2]]))]
							
							
							

							
							
							
							
							
							
							
							
							
							
							
							
							
							