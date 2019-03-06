{-# LANGUAGE InstanceSigs #-}

module Part3
  ( DayOfWeek
  , nextDay
  , afterDays
  , isWeekend
  , daysToParty

-- task 2 here

  , Nat
  , fromNatToInt
  , isEven
  , divide
  , remainder

  , Tree
  , isTreeEmpty
  , sizeTree
  , insertInTree
  , fromList
  , removeFromTree
  ) where


-- task 1
data DayOfWeek
  = Monday
  | Tuesday
  | Wednsday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

nextDay :: DayOfWeek -> DayOfWeek
nextDay Monday   = Tuesday
nextDay Tuesday  = Wednsday
nextDay Wednsday = Thursday
nextDay Thursday = Friday
nextDay Friday   = Saturday
nextDay Saturday = Sunday
nextDay Sunday   = Monday

afterDays :: DayOfWeek -> Int -> DayOfWeek
afterDays day 0    = day
afterDays day left = afterDays (nextDay day) (left - 1)

isWeekend :: DayOfWeek -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

daysToParty :: DayOfWeek -> Int
daysToParty Friday = 0
daysToParty day    = 1 + daysToParty (nextDay day)

-- task 2

-- data NonEmptyList a = Elem a | Cons a (NonEmptyList a)
--
-- data Castle = Castle
-- data Wall = Wall
-- data Lord = Lord
--
-- data City = City (Maybe Castle) (Maybe Lord) (Maybe Wall)



-- task 3
data Nat
  = Z
  | S Nat
  deriving (Show)

-- реализовал Num, чтобы удобнее использовать операции,
-- операции которые не понятно как должны вести оставил неопределенными
instance Num Nat where
  (+) a Z     = a
  (+) a (S b) = S (a + b)

  (*) _ Z     = 0
  (*) a (S Z) = a
  (*) a (S b) = a + (a * b)

  (-) Z _         = Z
  (-) a Z         = a
  (-) (S a) (S b) = (a - b)

  fromInteger 0 = Z
  fromInteger a
    | a < 0 = undefined
    | otherwise = S $ fromInteger $ a - 1

  abs = id
  signum = undefined
  negate = undefined


fromNatToInt :: Nat -> Int
fromNatToInt Z     = 0
fromNatToInt (S a) = (1 ) + (fromNatToInt a)


instance Eq Nat where
  (==) Z Z         = True
  (==) (S a) (S b) = a == b
  (==) _ _         = False


instance Ord Nat where
  (<=) Z _         = True
  (<=) _ Z         = False
  (<=) (S a) (S b) = a <= b


isEven :: Nat -> Bool
isEven Z         = True
isEven (S Z)     = False
isEven (S (S a)) = isEven a


-- iterSub :: Nat -> Nat -> Nat
-- iterSub Z b = Z
-- iterSub a b = S $ iterSub (a - b) b

divide :: Nat -> Nat -> Nat
divide _ Z = error "ban"
divide a b = (iterSub Z (S a)) - 1
  where iterSub acc t
          | t == Z    = acc
          | otherwise = iterSub (S acc) (t - b)


remainder :: Nat -> Nat -> Nat
remainder a b = a - (a `divide` b) * b


-- task 4
data Tree a
  = Leaf
  | Node [a] (Tree a) (Tree a)
  deriving (Show)

isTreeEmpty :: Tree a -> Bool
isTreeEmpty Leaf = True
isTreeEmpty _    = False

sizeTree :: Tree a -> Int
sizeTree Leaf = 0
sizeTree (Node list left right) = (length list) + (sizeTree left) + (sizeTree right)

insertInTree :: Ord a => Tree a -> a -> Tree a
insertInTree Leaf value = Node [value] Leaf Leaf
insertInTree (Node val@(x : _) left right) value =
  case compare value x of
    LT -> Node val (insertInTree left value) right
    EQ -> Node (value : val) left right
    GT -> Node val left (insertInTree right value)
insertInTree _ _ = error "Unreachable"

fromList :: Ord a => [a] -> Tree a
fromList []       = Leaf
fromList (x : xs) = insertInTree (fromList xs) x

-- находит самый левый элемент в поддереве и возвращает пару
-- из значения этой вершины и поддерева с удаленной самой левой вершиной
getMin :: Tree a -> ([a], Tree a)
getMin (Node val Leaf right) = (val, right)
getMin (Node val left right) =
  let (v, tree) = getMin left
  in (v, Node val tree right)
getMin _ = error "Unreachable"

removeFromTree :: Ord a => Tree a -> a -> Tree a
removeFromTree Leaf _ = Leaf
removeFromTree (Node (val : []) left Leaf) value | val == value = left
removeFromTree (Node (val : []) left right) value | val == value =
  let (v, tree) = getMin right
  in Node v left tree
removeFromTree (Node val@(x : xs) left right) value =
  case compare value x of
    LT -> Node val (removeFromTree left value) right
    EQ -> Node xs left right
    GT -> Node val left (removeFromTree right value)
removeFromTree _ _ = error "Unreachable"


-- Part4 task1.2
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Leaf = z
  foldr f z (Node val left right) = foldr f (foldr f (foldr f z right) val) left

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f (Node val left right) = foldMap f left `mappend` foldMap f val `mappend` foldMap f right
