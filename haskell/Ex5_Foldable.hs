module Ex5 where

-- we've seen classes and how we can implement methods
-- from them. Is there any particular method that is
-- useful to have sometimes?

-- Foldable
-- (many data structures work nicely with folds)
-- `foldl` can be expressed in terms of `foldr`
-- as `foldr` can work on infinite lists, while
-- `foldl` cannot.
-- FOLD <binary function> <accumulator> <data>
-- foldl f a bs = foldr (\b g x -> g(f x b)) id bs a

-- Requires:
-- 1. A container
-- 2. A binary operation f that takes the current value and the accumulator
-- 3. A starting value z

-- our old friend, (Binary) Tree
data Tree a
  = Empty
  | Leaf a
  | Branch (Tree a) (Tree a)
  deriving (Eq, Show)

-- a foldr function
treeFoldr f z Empty = z -- return the initial accum.
treeFoldr f z (Leaf x) = f x z -- apply f to the value of leaf
treeFoldr f z (Branch l r) = treeFoldr f (treeFoldr f z r) l

instance Foldable Tree where
  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr = treeFoldr

-- added by me: testing if ordering matters

commutativeOp a b = a + b

nonCommutativeOp a b = a - b

treeFoldrSwapped f z Empty = z
treeFoldrSwapped f z (Leaf x) = f z x -- HERE SWAPPED
treeFoldrSwapped f z (Branch l r) = treeFoldrSwapped f (treeFoldrSwapped f z r) l

tree = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
nonCommNormal = treeFoldr nonCommutativeOp 10 tree
nonCommSwapped = treeFoldrSwapped nonCommutativeOp 10 tree
commNormal = treeFoldr commutativeOp 10 tree
commSwapped = treeFoldrSwapped commutativeOp 10 tree

-- as expected: with commutative operators, changing the foldr definition (with more natural swapped z & x) is equivalent to original definition
-- while when using non commutative operators (such as minus) it DOES make a difference, so the swapped foldr definition is WRONG!
