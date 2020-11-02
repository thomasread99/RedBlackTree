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

  You are allowed to define any other auxiliary function you need.test

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
--isBST :: Ord a => RBT a -> Bool


-- Check the Black-balancing condition:
--     all paths have the same number of black nodes

--blackBalanced :: RBT a -> Bool


-- Black height of a black-balanced tree, -1 if not black-balanced

--blackHeight :: RBT a -> Int


-- Check if all Red-Black Tree conditions are satisfied
--isRBT :: Ord a => RBT a -> Bool


-- Insert a new element in a RBT, preserving the RBT properties

--insertRB :: Ord a => a -> RBT a -> RBT a


-- Delete an element from a RBT, preserving the RBT properties

--deleteRB :: Ord a => a -> RBT a -> RBT a