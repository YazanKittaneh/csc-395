module T where

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)

countLeaves :: Tree a -> Int
countLeaves Leaf = 1
countLeaves (Node t1 _ t2) = countLeaves t1 + countLeaves t2

treeAddOne :: Tree Int -> Tree Int
treeAddOne Leaf = Leaf
treeAddOne (Node t1 x t2) =
  Node (treeAddOne t1) (x + 1) (treeAddOne t2)

treeMap :: (a -> b) -> Tree a -> Tree a
treeMap f Leaf = f Leaf
treeMap f (Node t1 x t2) =
  Node (treeMap f t1) (f x) (treeMap f t2) --why do we have to make it node?

  treeMap'  :: (a -> b) -> Tree a -> Tree a
  treeMap' f = go
    where
      go Leaf = Leaf
      go (Node t1 x t2) = Node (go f t1) (f x) (go f t2)

data Maybe a = Nothing | Just a

maybeMap :: (a -> b) -> Maybe a => Maybe b
maybeMap Nothing
maybeMap (Just x) = Just $ f x


data Html a =
    Body [Html a]
  | P [Html a]
  | Strong [Html a]
  | Em [Html a]
  | A String [Html a]
  | Br
  | Raw a

html1 = Body (Strong (Raw "Alex Mitchell"))
html2 = Body (P (Br))
