{- darts Senan d'Art -}
module Ex03 where
  import Data.List ((\\))
  
  -- Datatypes -------------------------------------------------------------------
  
  -- do not change anything in this section !
  
  -- Binary Tree
  data BT a b
    = Leaf
    | Branch (BT a b) a b (BT a b)
    deriving (Eq, Show)
  
  -- association list
  type Assoc a b = [(a,b)]
  
  -- lookup binary (search) tree
  lkpBST :: Ord a1 => BT a1 a -> a1 -> Maybe a
  lkpBST Leaf _  =  Nothing
  lkpBST (Branch left k d right) k'
   | k < k'     =  lkpBST left k'
   | k > k'     =  lkpBST right k'
   | otherwise  =  Just d
  
  -- Coding Part 1 (13 Marks)
  
  -- insert into binary (search) tree
  insBST :: Ord a => a -> b -> BT a b -> BT a b
  insBST a b Leaf = Branch Leaf a b Leaf --if inserting into empty tree (or adding a new node), set the value of the node to the key/val
  insBST a b (Branch left k d right) --recursively step through the tree to find where the new pair should go
    | a < k     = Branch (insBST a b left) k d right
    | a > k     = Branch left k d (insBST a b right)
    | otherwise = Branch left a b right
  
  -- Coding Part 2 (6 Marks)
  
  -- convert an association list to a binary search tree
  assoc2bst :: Ord a => Assoc a b -> BT a b
  assoc2bst [] = Leaf --empty array is empty tree
  assoc2bst [(a, b)] = insBST a b Leaf -- adding a single node means inserting into an empty tree
  assoc2bst ((a,b):as) = insBST a b (assoc2bst as) --recursively add nodes until the end of the list is reached
  
  -- Coding Part 3 (6 Marks)
  
  -- convert a binary search tree into an (ordered) association list
  bst2assoc :: Ord c =>  BT c e -> Assoc c e
  bst2assoc Leaf = [] --empty tree is empty array
  bst2assoc (Branch left a b right) = concat[bst2assoc left, [(a,b)], bst2assoc right] --concat each of the nodes into an array in order