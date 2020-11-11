{-
  G54PAD Project in Advanced Algorithms and Data Structures
    Autumn 2020

  Assignment 1 
     Red-Black Trees

  Student Name: Thomas Read
  Student ID: 4296726

  Complete this Haskell file by providing definitions
  of the following functions:

    searchRB

    minRB
    maxRB

    isBST

    blackBalanced
    blackHeight

    insertRB
    deleteRB

  You are allowed to define any other auxiliary function you need.

-}

module RedBlack where

-- Definition of the Red-Black Tree data structure
data Color = Red | Black
  deriving (Eq,Show)

data RBT a = LeafRB | NodeRB Color (RBT a) a (RBT a)
  deriving (Eq,Show)


-- Serching a key inside a red-black tree
--   return True if the key is found, False otherwise
searchRB :: Ord a => a -> RBT a -> Bool
searchRB _ LeafRB = False
searchRB n (NodeRB c l x r) = 
  (n == x) || if (n < x) then searchRB n l
                         else searchRB n r


-- Minimum and maximum of red-black tree
--   return Nothing if the tree is empty
minRB :: RBT a -> Maybe a
minRB LeafRB = Nothing
minRB (NodeRB c LeafRB x r) = Just x
minRB (NodeRB c l x r) = minRB l

maxRB :: RBT a -> Maybe a
maxRB LeafRB = Nothing
maxRB (NodeRB c l x LeafRB) = Just x
maxRB (NodeRB c l x r) = maxRB r
  

-- Check if a tree satisfies the Binary Search Tree condition
--   (do not check other RBT conditions)
isBST :: Ord a => RBT a -> Bool
isBST t = boundCheck Nothing t Nothing

boundCheck :: Ord a => Maybe a -> RBT a -> Maybe a -> Bool
boundCheck l LeafRB r = l `checkSubtree` r
boundCheck l2 (NodeRB c l x r) r2 = 
  boundCheck l2 l (Just x) && boundCheck (Just x) r r2

checkSubtree :: Ord a => Maybe a -> Maybe a -> Bool
checkSubtree (Just x) (Just y) = x <= y
checkSubtree _ _ = True


-- Check the Black-balancing condition:
--     all paths have the same number of black nodes
blackBalanced :: RBT a -> Bool
blackBalanced LeafRB = True
blackBalanced (NodeRB c l x r) = 
  let diff = abs (countBlack l - countBlack r) in
      diff == 0 && blackBalanced l && blackBalanced r

--Auxillary function to count the number of black nodes in each subtree
countBlack :: RBT a -> Int
countBlack LeafRB = 0
countBlack (NodeRB c l x r) =
  if (c == Black) then countBlack l + 1
                  else countBlack l + 0


-- Black height of a black-balanced tree, -1 if not black-balanced
blackHeight :: RBT a -> Int
blackHeight LeafRB = 0
blackHeight (NodeRB c l x r) = 
  if (blackBalanced (NodeRB c l x r)) then (if (c == Black) then (countBlack l) + 1 else countBlack l)
                                      else -1


-- Check if all Red-Black Tree conditions are satisfied
isRBT :: Ord a => RBT a -> Bool
isRBT (NodeRB c l x r) = 
  if (c == Black) && (isBST (NodeRB c l x r)) && (blackBalanced (NodeRB c l x r)) then checkNodes l && checkNodes r                                        
                                                                                  else False

--Auxillary function to check for consecutive red nodes
checkNodes :: Ord a => RBT a -> Bool
checkNodes LeafRB = True
checkNodes (NodeRB c l x r) =  
  if (c == Red) then (if (isBlack l && isBlack r) then checkNodes l && checkNodes r else False)
                else checkNodes l && checkNodes r

--Auxillary function to check if a node is black
isBlack :: Ord a => RBT a -> Bool
isBlack LeafRB = True
isBlack (NodeRB c l x r) = 
  if (c == Black) then True
                  else False


-- Insert a new element in a RBT, preserving the RBT properties
insertRB :: Ord a => a -> RBT a -> RBT a
insertRB n x = blackRoot (ins n x)

ins :: Ord a => a -> RBT a -> RBT a
ins n LeafRB = (NodeRB Red (LeafRB) n (LeafRB))
ins n t@(NodeRB c l x r)
               | n < x = balance c (ins n l) x r
               | n > x = balance c l x (ins n r)
               | otherwise = t

--Auxillary function to turn a node black
blackRoot :: Ord a => RBT a -> RBT a
blackRoot LeafRB = LeafRB
blackRoot (NodeRB c l x r) =
  if c == Black then (NodeRB c l x r)
                else (NodeRB Black l x r)

--Auxillary function to turn a node red
redRoot :: Ord a => RBT a -> RBT a
redRoot LeafRB = LeafRB
redRoot (NodeRB c l x r) =
  if c == Red then (NodeRB c l x r)
              else (NodeRB Red l x r)

balance :: Color -> RBT a -> a -> RBT a -> RBT a
balance Black (NodeRB Red (NodeRB Red l x r) x2 r2) x3 r3
  = NodeRB Red (NodeRB Black l x r) x2 (NodeRB Black r2 x3 r3)  
balance Black (NodeRB Red l x (NodeRB Red l2 x2 r)) x3 r2
  = NodeRB Red (NodeRB Black l x l2) x2 (NodeRB Black r x3 r2)
balance Black l x (NodeRB Red (NodeRB Red l2 x2 r) x3 r2)
  = NodeRB Red (NodeRB Black l x l2) x2 (NodeRB Black r x3 r2)
balance Black l x (NodeRB Red l2 x2 (NodeRB Red l3 x3 r))
  = NodeRB Red (NodeRB Black l x l2) x2 (NodeRB Black l3 x3 r)
balance c l x r = NodeRB c l x r


-- Delete an element from a RBT, preserving the RBT properties
deleteRB :: Ord a => a -> RBT a -> RBT a
deleteRB n x = if (searchRB n x) then blackRoot (del n x)
                                 else x 

del :: Ord a => a -> RBT a -> RBT a
del n LeafRB = LeafRB
del n (NodeRB _ l x r)
             | n < x = delL n l x r
             | n > x = delR n l x r
             | otherwise = fuse l r

delL :: Ord a => a -> RBT a -> a -> RBT a -> RBT a
delL n l x r =
  if (color l) == Black then balL (del n l) x r
                        else NodeRB Red (del n l) x r

delR :: Ord a => a -> RBT a -> a -> RBT a -> RBT a
delR n l x r =
  if (color r) == Black then balR l x (del n r)
                        else NodeRB Red l x (del n r)

balL :: Ord a => RBT a -> a -> RBT a -> RBT a
balL (NodeRB Red l x r) x2 r2 =
  NodeRB Red (NodeRB Black l x r) x2 r2
balL l x (NodeRB Black l2 x2 r) =
  balance Black l x (NodeRB Red l2 x2 r)
balL l x (NodeRB Red (NodeRB Black l2 x2 r) x3 r2) =
  NodeRB Red (NodeRB Black l x l2) x2 (balance Black r x3 (redRoot r2))

balR :: Ord a => RBT a -> a -> RBT a -> RBT a
balR l x (NodeRB Red l2 x2 r) =
  NodeRB Red l x (NodeRB Black l2 x2 r)
balR (NodeRB Black l x r) x2 r2 =
  balance Black (NodeRB Red l x r) x2 r2
balR (NodeRB Red l x (NodeRB Black l2 x2 r)) x3 r3 =
  NodeRB Red (balance Black (redRoot l) x l2) x2 (NodeRB Black r x3 r3)

fuse :: Ord a => RBT a -> RBT a -> RBT a
fuse LeafRB x = x
fuse x LeafRB = x
fuse t1@(NodeRB Black l x r) (NodeRB Red l2 x2 r2) = NodeRB Red (fuse t1 l2) x2 r2
fuse (NodeRB Red l x r) t3@(NodeRB Black l2 x2 r2) = NodeRB Red l x (fuse r t3)
fuse (NodeRB Red l x r) (NodeRB Red l2 x2 r2) =
  let s = fuse r l2
  in case s of
    (NodeRB Red s1 y s2) -> (NodeRB Red (NodeRB Red l x s1) y (NodeRB Red s2 x2 r2))
    (NodeRB Black s1 y s2) -> balR (NodeRB Black s x2 r2) x r
    LeafRB -> (NodeRB Red l x (NodeRB Red s x2 r2))
fuse (NodeRB Black l x r) (NodeRB Black l2 x2 r2) =
  let s = fuse r l2
  in case s of
    (NodeRB Red s1 y s2) -> (NodeRB Red (NodeRB Black l x s1) y (NodeRB Black s2 x2 r))
    (NodeRB Black s1 y s2) -> balL l x (NodeRB Black s x2 r2)
    LeafRB -> (NodeRB Red l x (NodeRB Red s x2 r2))

--Auxillary function to get the colour of a node
color :: Ord a => RBT a -> Color
color LeafRB = Black
color (NodeRB c l x r) = c