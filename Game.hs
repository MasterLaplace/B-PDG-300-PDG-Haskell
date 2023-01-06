{-
-- EPITECH PROJECT, 2023
-- B-PDG-300-REN-3-1-PDGD03-guillaume.papineau
-- File description:
-- Game
-}

data Item = Sword | Bow | MagicWand deriving (Eq)

instance Show Item where
    show Sword = "sword"
    show Bow = "bow"
    show MagicWand = "magic wand"

data Mob = Mummy
    | Skeleton Item 
    | Wicth (Maybe Item)
    deriving (Eq)

createMummy :: Mob -- a Mummy
createMummy = Mummy

createArcher :: Mob -- a Skeleton holding a Bow
createArcher = Skeleton Bow

createKnight :: Mob -- a Skeleton holding a Sword
createKnight = Skeleton Sword

createWitch :: Mob -- a Witch holding Nothing
createWitch = Wicth Nothing

createSorceress :: Mob -- a Witch holding a MagicWand
createSorceress = Wicth (Just MagicWand)

myStrCmp :: String -> String -> Bool
myStrCmp [] [] = True
myStrCmp [] _ = False
myStrCmp _ [] = False
myStrCmp (x:xs) (y:ys) | x == y = myStrCmp xs ys
                    | otherwise = False

myInList :: [String] -> String -> Bool
myInList _ [] = error "empty arg"
myInList [] _ = False
myInList (x:xs) (y:ys) | myStrCmp x (y:ys) = True
                    | otherwise = myInList xs (y:ys)

create :: String -> Maybe Mob
create "mummy" = Just createMummy
create "doomed archer" = Just createArcher
create "dead knight" = Just createKnight
create "witch" = Just createWitch
create "sorceress" = Just createSorceress
create _ = Nothing

equip :: Item -> Mob -> Maybe Mob
equip x (Skeleton item2) = Just (Skeleton x)
equip x (Wicth mbItem) = Just (Wicth (Just x))
equip _ _ = Nothing

instance Show Mob where
    show Mummy = "mummy"
    show (Skeleton Bow) = "doomed archer"
    show (Skeleton Sword) = "dead knight"
    show (Skeleton i) = "skeleton holding a " ++ show i
    show (Wicth Nothing) = "witch"
    show (Wicth (Just MagicWand)) = "sorceress"
    show (Wicth (Just i)) = "witch holding a " ++ show i

class HasItem f where
    getItem :: f -> Maybe Item
    hasItem :: f -> Bool
    hasItem x | getItem x == Nothing = False
            | otherwise = True

instance HasItem Mob where
    getItem (Skeleton item) = Just item
    getItem (Wicth maybitem) = maybitem
    getItem _ = Nothing
