import Test.HUnit
import Test.QuickCheck
import TreeTest
import Tree
import MatrixQC
import MatrixTest
import Matrix

main =   do
      runTestTT tests 
      runTestTT matrixtests