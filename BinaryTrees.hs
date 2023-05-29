data Tree a =
    Leaf | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

node :: a -> Tree a
node x = Node x Leaf Leaf

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Leaf = node x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x Leaf = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

-- Tree generator from a list of nums
treeright :: (Ord a) => [a] -> Tree a
treeright [] = Leaf
treeright lst = foldr treeInsert Leaf lst

treeleft lst = treeright (reverse lst)

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

-- Balanced Binary Tree
isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node root Leaf Leaf) = True
isBalanced (Node root left right) = 
    case abs (depth left - depth right) of
      0 -> True
      1 -> True
      _ -> False
