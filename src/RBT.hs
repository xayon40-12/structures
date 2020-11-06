{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
module RBT where
    -- Red Black Tree
import GHC.Exts (IsList,Item,toList,fromList)

data Colour = R | B deriving (Eq, Ord, Show)
data RBT a = E | T Colour (RBT a) a (RBT a) deriving (Eq, Ord, Show)

instance (Ord a) => IsList (RBT a) where
    type Item (RBT a) = a

    fromList :: [a] -> RBT a
    fromList = foldr insert E

    toList :: RBT a -> [a]
    toList E = []
    toList (T _ tl v tr) = toList tl ++ v:toList tr

insert :: (Ord a) => a -> RBT a -> RBT a
insert x t = T B a b c
    where (T _ a b c) = ins t
          ins E = T R E x E
          ins t@(T c tl v tr) | x > v = balance(T c tl v (ins tr))
                              | x < v = balance(T c (ins tl) v tr)
                              | otherwise = t

balance :: RBT a -> RBT a
balance (T B (T R (T R a b c) d e) f g) = T R (T B a b c) d (T B e f g)
balance (T B (T R a b (T R c d e)) f g) = T R (T B a b c) d (T B e f g)
balance (T B a b (T R (T R c d e) f g)) = T R (T B a b c) d (T B e f g)
balance (T B a b (T R c d (T R e f g))) = T R (T B a b c) d (T B e f g)
balance x = x
