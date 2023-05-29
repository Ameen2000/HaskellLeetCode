data Tree a =
    Leaf | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- Find the depth of a Binary Tree
depth :: Tree a -> Int
depth Leaf = 0
depth (Node x Leaf Leaf) = 1
depth (Node x left right) =
    max (depth left + 1) (depth right + 1)

-- Find the diameter of a binary tree
dfs :: Tree a -> Int -> Int
dfs Leaf i = i
dfs (Node x left right) i =
    let leftDiameter = dfs left i
        rightDiameter = dfs right i
        in
        1 + max leftDiameter rightDiameter

treeDiameter :: Tree a -> Int
treeDiameter Leaf = -1
treeDiameter (Node x Leaf Leaf) = 0
treeDiameter (Node x left Leaf) = dfs (Node x left Leaf) (-1)
treeDiameter (Node x Leaf right) = dfs (Node x Leaf right) (-1)
treeDiameter (Node x left right) = dfs (Node x left right) 0

-- Invert Binary Tree
invert :: Tree a -> Tree a
invert Leaf = Leaf
invert (Node root Leaf Leaf) = Node root Leaf Leaf 
invert (Node root left right) = Node root (invert right) (invert left)
