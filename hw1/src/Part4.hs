{-# LANGUAGE InstanceSigs #-}

module Part4 where

-- task 1
data Pair a = Pair a a

instance Foldable Pair where
  foldr :: (a -> b -> b) -> b -> Pair a -> b
  foldr f z (Pair first second) = f first $ f second z

  foldMap :: Monoid m => (a -> m) -> Pair a -> m
  foldMap f (Pair first second) = (f first) `mappend` (f second)


data NonEmpty a = a :| [a]

instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f z (x :| []) = f x z
  foldr f z (x :| xs) = f x $ foldr f z xs

  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (x :| []) = f x
  foldMap f (x :| xs) = f x `mappend` foldMap f xs


-- task 1.2 in Part3
