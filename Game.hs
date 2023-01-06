data Item = Sword | Bow | MagicWand deriving (Eq)

instance Show Item where
    show :: Item -> String
    show Sword = "sword"
    show Bow = "bow"
    show MagicWand = "magic wand"

data Mob = Mummy
    | Skeleton { item :: Item }
    | Wicth { maybitem :: Maybe Item}
    deriving (Eq, Show)

createMummy :: Mob -- a Mummy
createMummy = Mummy

createArcher :: Mob -- a Skeleton holding a Bow
createArcher = Skeleton {item=Bow}

createKnight :: Mob -- a Skeleton holding a Sword
createKnight = Skeleton {item=Sword}

createWitch :: Mob -- a Witch holding Nothing
createWitch = Wicth {maybitem=Nothing}

createSorceress :: Mob -- a Witch holding a MagicWand
createSorceress = Wicth {maybitem=Just MagicWand}

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
equip item Mummy = Nothing
equip x (Skeleton item2) = Just (Skeleton item2) {item=x}
equip x (Wicth mbItem) = Just (Wicth mbItem) {maybitem=Just x}
