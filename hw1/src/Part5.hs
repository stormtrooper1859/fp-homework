{-# LANGUAGE InstanceSigs #-}

module Part5 where


-- task 1.1
maybeConcat :: [(Maybe [a])] -> [a]
maybeConcat = foldMap (maybe [] id)


-- task 2.1
data NonEmpty a = a :| [a]

instance Semigroup (NonEmpty a) where
  (<>) :: (NonEmpty a) -> (NonEmpty a) -> (NonEmpty a)
  (<>) (x :| xs) (b :| bs) = (x :| (xs <> [b] <> bs))


data ThisOrThat a b = This a | That b | Both a b

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
  (<>) :: (ThisOrThat a b) -> (ThisOrThat a b) -> (ThisOrThat a b)
  (<>) (This a1) (This a2)       = This (a1 <> a2)
  (<>) (This a) (That b)         = Both a b
  (<>) (This a1) (Both a2 b)     = Both (a1 <> a2) b
  (<>) (That b) (This a)         = Both a b
  (<>) (That b1) (That b2)       = That (b1 <> b2)
  (<>) (That b1) (Both a b2)     = Both a (b1 <> b2)
  (<>) (Both a1 b) (This a2)     = Both (a1 <> a2) b
  (<>) (Both a b1) (That b2)     = Both a (b1 <> b2)
  (<>) (Both a1 b1) (Both a2 b2) = Both (a1 <> a2) (b1 <> b2)


-- task 2.2

data Name = Name String deriving (Show)

instance Semigroup Name where
  (<>) (Name a) (Name b) = Name $ a <> "." <> b

instance Monoid Name where
  mempty = Name ""
  mappend (Name a) (Name b) = Name $ a `mappend` "." `mappend` b


newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
  (<>) end1 end2 = Endo $ (getEndo end2) . (getEndo end1)

instance Monoid (Endo a) where
  mempty = Endo id
  mappend end1 end2 = Endo $ (getEndo end2) . (getEndo end1)
