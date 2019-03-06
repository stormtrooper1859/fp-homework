{-# LANGUAGE TypeOperators #-}

module Part1
  ( associator
  , distributivity
  , eitherAssoc
  ) where


type (<->) a b = (a -> b, b -> a)


distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left a)       = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)


associator :: (a, (b, c)) -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)


assocLeft :: Either a (Either b c) -> Either (Either a b) c
assocLeft (Left a)          = Left (Left a)
assocLeft (Right (Left b))  = Left (Right b)
assocLeft (Right (Right c)) = Right c

assocRight :: Either (Either a b) c -> Either a (Either b c)
assocRight (Left (Left a))  = Left a
assocRight (Left (Right b)) = Right (Left b)
assocRight (Right c)        = Right (Right c)

eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (assocLeft, assocRight)
