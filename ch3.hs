module Ch3 where

data HeapT a = Empty | Tree Int a (HeapT a) (HeapT a)

class Heap h where
    empty :: h a
    isEmpty :: h a -> Bool

    insert :: (Ord a) => a -> h a -> h a
    merge :: (Ord a) => h a -> h a -> h a

    findMin :: (Ord a) => h a -> a
    deleteMin :: (Ord a) => h a -> h a

instance Heap HeapT where
    empty = Empty
    isEmpty Empty = True
    isEmpty _ = False

    insert = undefined

rank :: HeapT a -> Int
rank Empty = 0
rank (Tree r _ _ _) = r
