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
searchRB a (NodeRB c l x r) = 
  (a == x) || if (a < x) then searchRB a l
                         else searchRB a r

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
isBST t = boundBST Nothing t Nothing

boundBST :: Ord a => Maybe a -> RBT a -> Maybe a -> Bool
boundBST lb LeafRB rb = lb `leqM` rb
boundBST lb (NodeRB c l x r) rb = 
  boundBST lb l (Just x) && boundBST (Just x) r rb

leqM :: Ord a => Maybe a -> Maybe a -> Bool
leqM (Just x) (Just y) = x <= y
leqM _ _ = True

-- Check the Black-balancing condition:
--     all paths have the same number of black nodes

blackBalanced :: RBT a -> Bool
blackBalanced LeafRB = True
blackBalanced (NodeRB c l x r) = 
  let diff = abs (countBlack l - countBlack r) in
    diff == 0 && blackBalanced l && blackBalanced r


-- Black height of a black-balanced tree, -1 if not black-balanced
--MODIFY THIS, are leaf nodes counted as black nodes, is the root node counted if black?
blackHeight :: RBT a -> Int
blackHeight LeafRB = 0
blackHeight (NodeRB c l x r) = 
  if (blackBalanced (NodeRB c l x r)) then countBlack l
                                      else -1


countBlack :: RBT a -> Int
countBlack LeafRB = 0
countBlack (NodeRB c l x r) =
  if (c == Black) then countBlack l + countBlack r + 1
                  else countBlack l + countBlack r + 0


-- Check if all Red-Black Tree conditions are satisfied
--MODIFY THIS, check if is BST and that it is black balanced
isRBT :: Ord a => RBT a -> Bool
isRBT (NodeRB c l x r) = 
  --If the colour of the root is black then check the subtrees
  if (c == Black) then checkNodes l && checkNodes r
                  --Else it is not an RBT
                  else False

checkNodes :: Ord a => RBT a -> Bool
--If you reach a leaf then true
checkNodes LeafRB = True
checkNodes (NodeRB c l x r) =
  --If the colour is red and the children are black then check the subtrees, else false
  if (c == Red) then (if (isBlack l && isBlack r) then checkNodes l && checkNodes r else False)
                --Else check the subtree
                else checkNodes l && checkNodes r

isBlack :: Ord a => RBT a -> Bool
--Leaves are black
isBlack LeafRB = True
isBlack (NodeRB c l x r) = 
  --If the colour is black return true
  if (c == Black) then True
                  else False



-- Insert a new element in a RBT, preserving the RBT properties

insertRB :: Ord a => a -> RBT a -> RBT a
insertRB n LeafRB = (NodeRB Black (LeafRB) n (LeafRB))

-- Delete an element from a RBT, preserving the RBT properties

deleteRB :: Ord a => a -> RBT a -> RBT a
deleteRB n (NodeRB c l x r) = if (searchRB n (NodeRB c l x r)) then --Do a thing
                                                               else (NodeRB c l x r)