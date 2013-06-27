module Ch2 where

infixr 5 :-:

data CustomStack a =
    Nil | a :-: CustomStack a deriving (Show)

data Tree a = EmptyTree | Elem (Tree a) a (Tree a) deriving (Show)

class Stack st where
    emptyStack :: st a
    isEmpty :: st a -> Bool
    cons :: a -> st a -> st a
    head :: st a -> a
    tail :: st a -> st a
    (^++) :: st a -> st a -> st a
    update :: st a -> Int -> a -> st a

class Set s where
    emptySet :: s a
    insert :: (Eq a, Ord a) => a -> s a -> s a
    member :: (Eq a, Ord a) => a -> s a -> Bool

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

suffixes :: [a] -> [[a]]
suffixes xs = xs : case xs of
                     [] -> []
                     (_:xs') -> suffixes xs'

instance Set Tree where
    emptySet = EmptyTree

    insert x EmptyTree = Elem EmptyTree x EmptyTree
    insert x (Elem left a right)
        | x == a = Elem EmptyTree x EmptyTree
        | x < a  = Elem (insert x left) a right
        | x > a  = Elem left a (insert x right)

    member _ EmptyTree = False
    member x (Elem left a right)
        | x < a = member x left
        | otherwise = member' x right a
        where member' x' EmptyTree saved = x' == saved
              member' x' (Elem left' a' right') saved
                  | x' < a' = member' x' left' saved
                  | otherwise = member' x' right' a'
