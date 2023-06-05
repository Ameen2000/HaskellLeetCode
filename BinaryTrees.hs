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

nodeVal :: Tree a -> Maybe a
nodeVal Leaf = Nothing
nodeVal (Node a _ _) = Just a

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

-- Same Binary Tree
isSameTree :: (Eq a) => Tree a -> Tree a -> Bool
isSameTree Leaf Leaf = True
isSameTree (Node root1 left1 right1) (Node root2 left2 right2) =
    (root1 == root2) && isSameTree left1 left2 && isSameTree right1 right2
isSameTree Leaf _ = False
isSameTree _ Leaf = False

-- Subtree of another Tree
isSubTree :: (Eq a) => Tree a -> Tree a -> Bool
isSubTree _ Leaf = True
isSubTree Leaf _ = False
isSubTree (Node root left right) (Node subroot l r) =
    isSameTree (Node root left right) (Node subroot l r) || 
    isSubTree left (Node subroot l r) || isSubTree right (Node subroot l r)

-- Lowest Common Ancestor for Binary Search Tree
lcaBST :: (Ord a) => Tree a -> Tree a -> Tree a -> Tree a
lcaBST Leaf _ _ = Leaf
lcaBST (Node root left right) node1 node2 
    |  Just root > nodeVal node1 && Just root > nodeVal node2
    = lcaBST left node1 node2
    | Just root < nodeVal node1 && Just root < nodeVal node2
    = lcaBST right node1 node2
    | otherwise = Node root left right

-- Good Nodes of a Binary Tree
goodNodes :: (Ord a) => Tree a -> Int
goodNodes tr =
    let aux tr maxVal =
            case tr of
              Leaf -> 0
              Node val left right ->
                  if val >= maxVal
                     then 1 + aux left val + aux right val
                     else aux left val + aux right val
            in
            case tr of
              Leaf -> 0
              Node val _ _ -> aux tr val
