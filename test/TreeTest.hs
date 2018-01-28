module TreeTest where

import Test.HUnit
import Test.QuickCheck
import Tree
import MatrixQC
import MatrixTest
import Matrix

testTree1 = (Node 5 Nil Nil)
--5
testTree2 = (Node 8 (Node 5 Nil Nil) Nil)
--8
--5 $
testTree3 = (Node 9 (Node 8 (Node 5 Nil Nil) Nil) (Node 11 (Node 10 Nil Nil) Nil))
--9
--8 11
--5 $ 10 $

testTree4 = (Node 10 (Node 9 (Node 8 (Node 7 (Node 6 (Node 5 Nil Nil) Nil) Nil) Nil) Nil) Nil)


testquickSort = test [ "quickSort [5,43,21,6,5,5]" ~: [5,5,5,6,21,43] ~=? (quickSort [5,43,21,6,5,5]),
                       "quickSort [6,5,7,4,8,1,9,2,0,3]" ~: [0,1,2,3,4,5,6,7,8,9] ~=? (quickSort [6,5,7,4,8,1,9,2,0,3])
                     ]

                     
testdeleteTreeEl = test [ "deleteTreeEl testTree1 5" ~: Nil ~=? (deleteTreeEl testTree1 5),
                          "deleteTreeEl testTree2 8" ~: testTree1 ~=? (deleteTreeEl testTree2 8),
                          "deleteTreeEl (deleteTreeEl (deleteTreeEl (deleteTreeEl testTree3 10) 9) 11) 8" ~: 
                          testTree1 ~=? (deleteTreeEl (deleteTreeEl (deleteTreeEl (deleteTreeEl testTree3 10) 9) 11) 8)
                        ]
                              
                              
testaddTreeEl = test [ "addTreeEl Nil 5" ~: testTree1 ~=? (addTreeEl Nil 5),
                       "addTreeEl(addTreeEl Nil 8) 5" ~: testTree2 ~=? (addTreeEl(addTreeEl Nil 8) 5),
                       "addTreeEl (addTreeEl (addTreeEl (addTreeEl (addTreeEl Nil 9) 8) 11) 10" ~: 
                       testTree3 ~=? (addTreeEl (addTreeEl (addTreeEl (addTreeEl (addTreeEl Nil 9) 8) 11) 10) 5)
                     ]
                           
                           
                           
testtreeHeight = test [ "treeHeight Nil" ~: 0 ~=? (treeHeight Nil),
                        "treeHeight testTree1" ~: 1 ~=? (treeHeight testTree1),
                        "treeHeight testTree2" ~: 2 ~=? (treeHeight testTree2),
                        "treeHeight testTree3" ~: 3 ~=? (treeHeight testTree3),
                        "treeHeight testTree4" ~: 6 ~=? (treeHeight testTree4)
                      ]

                    
testflattenTree = test [ "flattenTree testTree1" ~: [5] ~=? (flattenTree testTree1),
                         "flattenTree testTree2" ~: [5,8] ~=? (flattenTree testTree2),
                         "flattenTree testTree3" ~: [5,8,9,10,11] ~=? (flattenTree testTree3),
                         "flattenTree testTree4" ~: [5,6,7,8,9,10] ~=? (flattenTree testTree4)
                       ]

 
testflattenTreeInOrder = test [ "flattenTreeInOrder testTree1" ~: [5] ~=? (flattenTreeInOrder testTree1),
                                "flattenTreeInOrder testTree2" ~: [8,5] ~=? (flattenTreeInOrder testTree2),
                                "flattenTreeInOrder testTree3" ~: [9,8,5,11,10] ~=? (flattenTreeInOrder testTree3),
                                "flattenTreeInOrder testTree4" ~: [10,9,8,7,6,5] ~=? (flattenTreeInOrder testTree4)
                              ]

    
testflattenTreePostOrder = test [ "flattenTreePostOrder testTree1" ~: [5] ~=? (flattenTreePostOrder testTree1),
                                  "flattenTreePostOrder testTree2" ~: [5,8] ~=? (flattenTreePostOrder testTree2),
                                  "flattenTreePostOrder testTree3" ~: [5,8,10,11,9] ~=? (flattenTreePostOrder testTree3),
                                  "flattenTreePostOrder testTree4" ~: [5,6,7,8,9,10] ~=? (flattenTreePostOrder testTree4)
                                ]

                    
testelementsLeftOfN = test [ "elementsLeftOfN [1,2,3] 2" ~: [1] ~=? (elementsLeftOfN [1,2,3] 2),
                             "elementsLeftOfN [1,1,1,4,4,4,6,6,6] 5" ~: [1,1,1,4,4,4] ~=? (elementsLeftOfN [1,1,1,4,4,4,6,6,6] 5),
                             "elementsLeftOfN [1,4,4,4,6,6,6] 4" ~: [1,4,4] ~=? (elementsLeftOfN [1,4,4,4,6,6,6] 4)
                           ]
 
 
testelementsRightOfN = test [ "elementsRightOfN [1,2,3] 2" ~: [3] ~=? (elementsRightOfN [1,2,3] 2),
                              "elementsRightOfN [1,1,1,4,4,4,6,6,6] 5" ~: [6,6,6] ~=? (elementsRightOfN [1,1,1,4,4,4,6,6,6] 5),
                              "elementsRightOfN [1,4,4,4,6,6,6] 4" ~: [6,6,6] ~=? (elementsRightOfN [1,4,4,4,6,6,6] 4) 
                            ]

                   
testdeleteFromList = test [ "deleteFromList [1,2,3] 2" ~: [1,3] ~=? (deleteFromList [1,2,3] 2),
                            "deleteFromList [1,1,1,4,4,4,6,6,6] 5" ~: [1,1,1,4,4,4,6,6,6] ~=? (deleteFromList [1,1,1,4,4,4,6,6,6] 5),
                            "deleteFromList [1,4,4,4,6,6,6] 4" ~: [1,6,6,6] ~=? (deleteFromList [1,4,4,4,6,6,6] 4) 
                          ]

                       
                     
testraiseTree = test [ "raiseTree [5]" ~: testTree1 ~=? (raiseTree [5]),
                       "raiseTree [5,8]" ~: (Node 5 Nil (Node 8 Nil Nil)) ~=? (raiseTree [5,8]),
                       "raiseTree [8,5]" ~: (Node 5 Nil (Node 8 Nil Nil)) ~=? (raiseTree [5,8]),
                       "raiseTree [9,8,5,10,11]" ~: (Node 9 (Node 5 Nil (Node 8 Nil Nil)) (Node 10 Nil (Node 11 Nil Nil))) ~=? (raiseTree [9,8,5,10,11])
                     ]

                    
testtreeContainsValue = test [ "treeContainsValue testTree1 5" ~: True ~=? (treeContainsValue testTree1 5),
                               "treeContainsValue testTree1 1" ~: False ~=? (treeContainsValue testTree1 1),
                               "treeContainsValue testTree2 8" ~: True ~=? (treeContainsValue testTree2 8),
                               "treeContainsValue testTree2 5" ~: True ~=? (treeContainsValue testTree2 5),
                               "treeContainsValue testTree2 2" ~: False ~=? (treeContainsValue testTree2 2),
                               "treeContainsValue testTree3 5" ~: True ~=? (treeContainsValue testTree3 11),
                               "treeContainsValue testTree3 5" ~: True ~=? (treeContainsValue testTree3 5),
                               "treeContainsValue testTree3 19" ~: False ~=? (treeContainsValue testTree3 19)
                             ]

                     
testrepairTree = test [ "repairTree testTree1" ~: testTree1 ~=? (repairTree testTree1),
                        "repairTree testTree2" ~: (Node 5 Nil (Node 8 Nil Nil)) ~=? (repairTree testTree2),
                        "repairTree testTree3" ~: (Node 9 (Node 5 Nil (Node 8 Nil Nil)) (Node 10 Nil (Node 11 Nil Nil))) 
                        ~=? (repairTree testTree3),
                        "repairTree testTree4" ~: (Node 7 (Node 5 Nil (Node 6 Nil Nil)) (Node 9 (Node 8 Nil Nil) (Node 10 Nil Nil))) 
                        ~=? (repairTree testTree4)
                      ]
 
 

testtreeElements = test [ "treeElements testTree1" ~: 1 ~=? (treeElements testTree1),
                          "treeElements testTree2" ~: 2 ~=? (treeElements testTree2),
                          "treeElements testTree3" ~: 5 ~=? (treeElements testTree3),
                          "treeElements testTree4" ~: 6 ~=? (treeElements testTree4)
                        ]

                      
testsumTree = test [ "sumTree testTree1" ~: 5 ~=? (sumTree testTree1),
                     "sumTree testTree2" ~: 13 ~=? (sumTree testTree2),
                     "sumTree testTree3" ~: 43 ~=? (sumTree testTree3),
                     "sumTree testTree4" ~: 45 ~=? (sumTree testTree4)
                   ]

                    
testmulTree = test [ "mulTree testTree1" ~: 5 ~=? (mulTree testTree1),
                     "mulTree testTree2" ~: 40 ~=? (mulTree testTree2),
                     "mulTree testTree3" ~: 39600 ~=? (mulTree testTree3),
                     "mulTree testTree4" ~: 151200 ~=? (mulTree testTree4) 
                   ]

testcombineTrees = test [ "combineTrees testTree1 testTree2" ~: (Node 5 (Node 5 Nil Nil) (Node 8 Nil Nil)) 
                          ~=? (combineTrees testTree1 testTree2),
                          "combineTrees testTree3 testTree4" ~: (repairTree (addTreeEl testTree3 5)) 
                          ~=? (combineTrees testTree3 testTree1)
                        ]

                      
testmapAndSumTree = test [ "mapAndSumTree (x -> 2*x + 1) testTree1" ~: 11
                           ~=? (mapAndSumTree (\x -> 2*x + 1) testTree1),
                           "mapAndSumTree (x -> x `div` 2) testTree2" ~: 6
                           ~=? (mapAndSumTree (\x -> x `div` 2) testTree2),
                           "mapAndSumTree (x -> x^2) testTree3" ~: 391
                           ~=? (mapAndSumTree (\x -> x^2) testTree3),
                           "mapAndSumTree (x -> x^2) testTree3" ~: ((treeElements testTree4) + (sumTree testTree4))
                           ~=? (mapAndSumTree (\x -> x + 1) testTree4)
                         ]

                    
testfoldrTree = test [ "foldrTree (+) 0 testTree1" ~: 5 ~=? (foldrTree (+) 0 testTree1),
                       "foldrTree (+) 0 testTree2" ~: 13 ~=? (foldrTree (+) 0 testTree2),
                       "foldrTree (+) 0 testTree3" ~: 43 ~=? (foldrTree (+) 0 testTree3),
                       "foldrTree (+) 0 testTree4" ~: 45 ~=? (foldrTree (+) 0 testTree4),
                       "foldrTree (*) 1 testTree2" ~: 40 ~=? (foldrTree (*) 1 testTree2)
                     ]

                      
testfoldrMapTree = test [ "foldrMapTree (x -> x^3) (+) 1 testTree2" ~: 637 ~=? (foldrMapTree (\x -> x^3) (+) 0 testTree2),
                          "foldrMapTree (x -> x-1) (*) 1 testTree3" ~: 20160 ~=? (foldrMapTree (\x -> x-1) (*) 1 testTree3)
                        ]


tests = TestList [TestLabel "test1" testdeleteTreeEl,
                  TestLabel "test2" testaddTreeEl,
                  TestLabel "test3" testtreeHeight,
                  TestLabel "testflattenTree" testflattenTree,
                  TestLabel "testquickSort" testquickSort,
                  TestLabel "testflattenTreeInOrder" testflattenTreeInOrder,
                  TestLabel "testflattenTreePostOrder" testflattenTreePostOrder,
                  TestLabel "testelementsLeftOfN" testelementsLeftOfN,
                  TestLabel "testelementsRightOfN" testelementsRightOfN,
                  TestLabel "testdeleteFromList" testdeleteFromList,
                  TestLabel "testraiseTree" testraiseTree,
                  TestLabel "testtreeContainsValue" testtreeContainsValue,
                  TestLabel "testrepairTree" testrepairTree,
                  TestLabel "testtreeElements" testtreeElements,
                  TestLabel "testsumTree" testsumTree,
                  TestLabel "testmulTree" testmulTree,
                  TestLabel "testcombineTrees" testcombineTrees,
                  TestLabel "testmapAndSumTree" testmapAndSumTree,
                  TestLabel "testfoldrTree" testfoldrTree,
                  TestLabel "testfoldrMapTree" testfoldrMapTree
                  ]

       
       
       