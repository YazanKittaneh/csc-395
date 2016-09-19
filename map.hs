import Prelude hiding (map, filter, fold)


map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = (f x):(map f xs)


filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs)
  | f x        = x : filter f xs
  | otherwilse = filter f xs'


foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ init [] = init
foldl f init (x:xs) = foldl f (f init x) xs

data Tree a =
  | Leaf
  | Node (Tree a) a (Tree a)

emptyTree :: Tree
emptyTree = Leaf

countLeaves :: Tree -> Int
countLeaves emptyTree = 1
countLeaves (Node r v 1)) = 
