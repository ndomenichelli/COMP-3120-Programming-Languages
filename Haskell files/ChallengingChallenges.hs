-- data Point = Point { x :: Int, y :: Int} deriving(Show)

-- instance Num point where  
 -- (+) (Point a b) (Point c d) = Point(a+b) (c+d)
 
 
data Tree a = EmptyTree 
 | Node {value :: a, left :: (Tree a), right :: (Tree a)} deriving (Show, Read, Eq)
 
instance Functor Tree where
 fmap f EmptyTree = EmptyTree
 fmap f (Node v left right) = Node (f v) (fmap f left) (fmap f right)

toList EmptyTree = [] 
toList (Node v left right) = v : (toList left) ++ (toList right) 
  
instance Foldable Tree where
 foldr f s EmptyTree = s
 -- foldr f s (Node v left right) = Node(f v) foldr f (f v  (foldr f s right)) left
 foldr f s tree = foldr f s (toList tree)

-- toList tree = foldr (:) [] tree

-- singleton :: a -> Tree a
-- singleton x = Node x EmptyTree EmptyTree