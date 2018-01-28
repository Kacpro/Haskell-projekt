module Tree where

data Tree a = Nil | 
              Node a (Tree a) (Tree a)
              deriving (Read)

instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Node a b c) = Node (f a) (fmap f b) (fmap f c)

instance Eq a => Eq (Tree a) where
  (==) (Nil) (Nil) = True
  (==) (Node a left right) (Nil) = False
  (==) (Nil) (Node a left right) = False
  (==) (Node a left1 right1) (Node b left2 right2) = (a == b && left1 == left2 && right1 == right2)
  
instance Show a => Show (Tree a) where
 show Nil = "Nil"
 show (Node a left right) = "(" ++ (show a) ++ " " ++ (show left) ++ " " ++ (show right) ++ ")"
  
exampleTree = Node 5 (Node 2 Nil Nil) (Node 9 (Node 6 Nil Nil) Nil)
  
-- | Returns tree that contains both given tree and given value
addTreeEl :: (Eq a, Ord a, Show a, Read a) => Tree a -> a -> Tree a
addTreeEl Nil el = Node el Nil Nil
addTreeEl (Node a left right) el = if el > a
                                   then Node a left (addTreeEl right el)
                                   else Node a (addTreeEl left el) right

-- | Returns height of a given tree
treeHeight :: Tree a -> Int
treeHeight Nil = 0
treeHeight (Node a left right) = 1 + max (treeHeight left) (treeHeight right)


-- | Returns sorted array, helper function
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort (lowerThanX xs) ++ [x] ++ quickSort (higherThanX xs)
  where
  lowerThanX xs = [y | y <- xs, y <= x]
  higherThanX xs = [y | y <- xs, y > x]
  
  
-- | Returns list that is representation of given Tree's elements
flattenTree :: (Show a) => Tree a -> [a]
flattenTree Nil = []
flattenTree (Node a left right) = (flattenTree left) ++ [a] ++ (flattenTree right)

-- | Returns list that is representation of given Tree's elements but in different order
flattenTreeInOrder :: (Show a) => Tree a -> [a]
flattenTreeInOrder Nil = []
flattenTreeInOrder (Node a left right) = [a] ++ (flattenTreeInOrder left) ++ (flattenTreeInOrder right)

-- | Returns list that is representation of given Tree's elements but in different order
flattenTreePostOrder :: (Show a) => Tree a -> [a]
flattenTreePostOrder Nil = []
flattenTreePostOrder (Node a left right) = (flattenTreePostOrder left) ++ (flattenTreePostOrder right) ++ [a]

-- | Returns list of elements that are lower or equal to element N (except for that one element), helper function
elementsLeftOfN :: (Ord a, Eq a) => [a] -> a -> [a]
elementsLeftOfN [] n       = []
elementsLeftOfN [x] n      = if (x < n) then [x]
                             else []
elementsLeftOfN [x,z] n    = if (x < n && z < n) then [x,z]
                             else if x < n then [x]
                             else if z == n then [x]
                             else []
elementsLeftOfN (x:z:xs) n = if(x <= n && z <= n) then [x] ++ (elementsLeftOfN (z:xs) n)
                             else if (x < n && z > n) then [x]
                             else []

                             
-- | Returns list of elements that are higher than a given element, helper function                           
elementsRightOfN :: (Ord a, Eq a) => [a] -> a -> [a]
elementsRightOfN [] n       = []
elementsRightOfN (x:xs) n = if(x > n) then (x:xs)
                            else elementsRightOfN xs n
     
    
-- | Returns list that does not contain given argument, helper function
deleteFromList :: (Ord a, Eq a) => [a] -> a -> [a]
deleteFromList [] n     = []
deleteFromList (x:xs) n = if x == n then deleteFromList xs n
                          else [x] ++ (deleteFromList xs n)


-- | Returns tree which elements are based on those from the list
raiseTree :: (Ord a) => [a] -> Tree a
raiseTree [] = Nil
raiseTree xs = Node
               middleEl
               (raiseTree (elementsLeftOfN sortedList middleEl))
               (raiseTree (elementsRightOfN sortedList middleEl))
    where
    middleEl = sortedList !! ((length xs - 1) `div` 2)
    sortedList = quickSort xs


-- | Returns tree that does not contain any elements equal to given argument
deleteTreeEl :: (Eq a, Ord a, Show a) => Tree a -> a -> Tree a
deleteTreeEl Nil n                 = Nil
deleteTreeEl t n =  raiseTree $ deleteFromList (flattenTree t) n


-- | Checks whether or not the tree contains given value
treeContainsValue :: (Eq a, Ord a) => Tree a -> a -> Bool
treeContainsValue Nil n = False
treeContainsValue (Node a left right) n = ((a == n) || treeContainsValue branch n)
                                          where 
                                          branch = if (a > n) then left
                                                   else right


-- | "Sorts" the tree when the elements are not in a proper order or balances the tree (by recovering the log(n) search time complexity)
repairTree :: (Eq a, Ord a, Show a) => Tree a -> Tree a
repairTree t = raiseTree $ flattenTree t


-- | Computates elements of a tree based on the given function (operator)
foldrTree :: (a -> a -> a) -> a -> Tree a -> a
foldrTree f z Nil = z
foldrTree f z (Node a right left) = f (f a (foldrTree f z right)) (foldrTree f z left)


-- | Computates elements of a tree based on the given function (operator) and additionally applies another function to every tree element
foldrMapTree :: (a -> a) -> (a -> a -> a) -> a -> Tree a -> a
foldrMapTree f g z tree = foldrTree g z $ fmap f tree 


-- | Applies given function to every tree element and computes the sum of a tree
mapAndSumTree f tree = foldrMapTree (f) (+) 0 tree


-- | Returns the number of elements a given tree has
treeElements = foldrMapTree (\x -> 1) (+) 0


-- | Returns the sum of tree elements
sumTree = foldrTree (+) 0


-- | Returns the product of a given tree elements
mulTree = foldrTree (*) 1


-- | Returns tree that is the combination of two given trees
combineTrees :: (Show a, Ord a) => Tree a -> Tree a -> Tree a
combineTrees a b = raiseTree $ quickSort $ flattenTree a ++ flattenTree b
















