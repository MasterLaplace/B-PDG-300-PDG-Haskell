{-
-- EPITECH PROJECT, 2023
-- B-PDG-300-REN-3-1-PDGD03-guillaume.papineau
-- File description:
-- Tree
-}

module Tree where

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

listToTree :: Ord a => [a] -> Tree a
listToTree [] = Empty
listToTree [x] = Node Empty x Empty
listToTree (x:xs) = addInTree x (listToTree xs)

treeToList :: Ord a => Tree a -> [a]
treeToList Empty = []
treeToList (Node left x right) = treeToList left ++ [x] ++ treeToList right

treeSort :: Ord a => [a]-> [a]
treeSort x = treeToList (listToTree x)

instance Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node a x b) = foldMap f a `mappend` f x `mappend` foldMap f b
