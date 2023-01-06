{-
-- EPITECH PROJECT, 2023
-- B-PDG-300-REN-3-1-PDGD03-guillaume.papineau
-- File description:
-- Tree
-}

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show)

addInTree :: Ord a => a -> Tree a -> Tree a
addInTree x Empty = Node Empty x Empty
addInTree x (Node left a right)
    | x == a = Node left x right
    | x < a  = Node (addInTree x left) a right
    | x > a  = Node left a (addInTree x right)

instance Functor Tree where
    fmap x Empty = Empty
    fmap x (Node left up right) = Node (fmap x left) (x up) (fmap x right)

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ x [] = x
myFoldr lambda x (y:ys) = lambda y (myFoldr lambda x ys)

listToTree :: Ord a => [a] -> Tree a
listToTree [] = Empty
listToTree [x] = Node Empty x Empty
listToTree (x:xs) | x < head xs = Node (listToTree xs) x Empty
                | otherwise = Node Empty x (listToTree xs)

treeToList :: (Ord a) => Tree a -> [a]
treeToList Empty = []
treeToList (Node left root right) = treeToList left ++ [root] ++ treeToList right


treeSort :: (Ord a) => [a]-> [a]
treeSort x = treeToList (listToTree x)

instance Foldable Tree where
    foldMap f = foldr (\x acc -> f x <> acc) mempty
