module MyTree

data Tree elem =
    Empty
  | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1


insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x tree@(Node left y right) = case compare x y of
                                    LT => Node (insert x left) y right
                                    EQ => tree
                                    GT => Node left y (insert x right)

total listToTree : Ord a => List a -> Tree a
listToTree = foldl (flip insert) Empty

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left x right) = treeToList left ++ [x] ++ treeToList right
