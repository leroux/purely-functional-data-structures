module Ch2 where

import Prelude hiding (lookup)

--- 2.1
suffixes :: [a] -> [[a]]
suffixes xs = xs : suffixes (Prelude.tail xs)

--- Stack
infixr 5 :-:

data CustomStack a = Nil | a :-: CustomStack a deriving (Show)

instance Show a => Show (Tree a) where
    show t = show' t 0
      where show' EmptyTree indent = replicate indent '\t' ++ "∅"
            show' (Elem left a right) indent = replicate indent '\t' ++ "(" ++ show a ++ "\n" ++ show' left (indent + 1) ++ "\n" ++ show' right (indent + 1) ++ ")"

class Stack st where
    emptyStack :: st a
    isEmpty :: st a -> Bool
    cons :: a -> st a -> st a
    head :: st a -> a
    tail :: st a -> st a
    (^++) :: st a -> st a -> st a
    update :: st a -> Int -> a -> st a

instance Stack [] where
    emptyStack = []

    isEmpty [] = True
    isEmpty _ = False

    cons x xs = x : xs

    head [] = error "head: empty stack"
    head (x:_) = x

    tail [] = error "tail: empty stack"
    tail (_:xs) = xs

    [] ^++ ys = ys
    (x:xs) ^++ ys = x : (xs ^++ ys)

    update [] _ _ = error "update: out of bounds"
    update (_:xs) 0 y = y : xs
    update (x:xs) i y = x : update xs (i - 1) y

instance Stack CustomStack where
    emptyStack = Nil

    isEmpty Nil = True
    isEmpty _ = False

    cons x xs = x :-: xs

    head Nil = error "head: empty stack"
    head (x :-: _) = x

    tail Nil = error "tail: empty stack"
    tail (_ :-: xs) = xs

    Nil ^++ ys = ys
    (x :-: xs) ^++ ys = x :-: (xs ^++ ys)

    update Nil _ _ = error "update: out of bounds"
    update (_ :-: xs) 0 y = y :-: xs
    update (x :-: xs) i y = x :-: update xs (i - 1) y

instance Functor CustomStack where
    fmap _ Nil = Nil
    fmap f (x :-: xs) = f x :-: fmap f xs

--- Tree, Set
data Tree a = EmptyTree | Elem (Tree a) a (Tree a)
type Set = Tree

class UnbalancedSet s where
    emptySet :: s a
    insert :: (Eq a, Ord a) => a -> s a -> s a
    member :: (Eq a, Ord a) => a -> s a -> Bool

instance UnbalancedSet Tree where
    emptySet = EmptyTree

    --- 2.3, 2.4
    insert x EmptyTree = Elem EmptyTree x EmptyTree
    insert x t@(Elem left a right)
      | x < a = Elem (insert x left) a right
      | otherwise = Elem left a (insert' right t)
      where insert' EmptyTree (Elem _ a' _)
              | x == a' = error "insert: element exists"
              | otherwise = Elem EmptyTree x EmptyTree
            insert' t'@(Elem left' a' right') candidate
              | x < a' = Elem (insert' left' candidate) a' right'
              | otherwise = Elem left' a' (insert' right' t')

    --- 2.2
    member _ EmptyTree = False
    member x (Elem left a right)
      | x < a = member x left
      | otherwise = member' right a
      where member' EmptyTree candidate = x == candidate
            member' (Elem left' a' right') candidate
              | x < a' = member' left' candidate
              | otherwise = member' right' a'

instance Functor Tree where
    fmap _ EmptyTree = EmptyTree
    fmap f (Elem left a right) = Elem (fmap f left) (f a) (fmap f right)

depth :: Tree a -> Int
depth EmptyTree = 0
depth (Elem left _ right) = 1 + max (depth left) (depth right)

--- 2.5a
complete :: a -> Int -> Tree a
complete _ 0 = EmptyTree
complete x d = Elem (complete x $ d - 1) x (complete x $ d - 1)

--- 2.5b
-- ???
---

--- 2.6
class FiniteMap m where
    empty :: m a
    bind :: (Eq k, Ord k) => k -> v -> m (k, v) -> m (k, v)
    lookup :: (Eq k, Ord k) => k -> m (k, v) -> Maybe v

instance FiniteMap Tree where
    empty = EmptyTree

    bind k v EmptyTree = Elem EmptyTree (k, v) EmptyTree
    bind k v t@(Elem left a@(k', _) right)
      | k < k' = Elem (bind k v left) a right
      | otherwise = Elem left a (bind' right t)
      where bind' EmptyTree (Elem _ (k1', _) _)
              | k == k1' = error "insert: key exists"
              | otherwise = Elem EmptyTree (k, v) EmptyTree
            bind' t'@(Elem left' a'@(k1', _) right') candidate
              | k < k1' = Elem (bind' left' candidate) a' right'
              | otherwise = Elem left' a' (bind' right' t')

    lookup = undefined
