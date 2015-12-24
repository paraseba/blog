{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Array
import Data.Monoid
import Data.List (genericLength)
import Data.Foldable

import Criterion.Main
import Test.QuickCheck
import Test.QuickCheck.Function

data Arr a = Arr {toArray :: !(Array Integer a)} deriving (Show, Eq)

fromList :: [a] -> Arr a
fromList [] = error "No empty arrays"
fromList as = Arr $ {-# SCC "mk-native-array" #-} listArray (0, genericLength as - 1) as

instance Functor Arr where
  fmap f = Arr . fmap f . toArray

instance Foldable Arr where
  foldMap f = foldMap f . toArray

instance Applicative Arr where
  pure =  fromList . pure
  fs <*> as = fromList [f a | f <- toList fs, a <- toList as]

instance Monad Arr where
  return = pure
  as >>= f = fromList $ concatMap (toList . f) as

monadWork :: Arr Int -> Int
monadWork as = sum $ do
  i <- as
  j <- as
  return (i + j)

applicativeWork :: Arr Int -> Int
applicativeWork as = sum $ (+) <$> as <*> as
  
instance Arbitrary a => Arbitrary (Arr a) where
  arbitrary = fromList . getNonEmpty <$> arbitrary
  
prop_equivMonadApplicative as =
  monadWork as == applicativeWork as

prop_sameAsLists (NonEmpty as) (NonEmpty bs) =
  toList withArray == withList
  where
    withList  = do {i <- as; j <- bs; return (i,j)} :: [(Int,Int)]
    withArray = do {i <- fromList as; j <- fromList bs; return (i,j)}

prop_functorId as =
  fmap id (as :: Arr Int) == as

prop_functorComp (Blind f) (Blind g) as =
  fmap (f . g) (as :: Arr Int) == (fmap f . fmap g) as

prop_monadLeftId (Blind k) a =
  ((return a :: Arr Int) >>= k) ==  k a

prop_monadRightId as =
  ((as :: Arr Int) >>= return) ==  as

prop_monadAssoc (Blind k) (Blind h) as =
  ((as :: Arr Int) >>= (\x -> k x >>= h)) == ((as >>= k) >>= h)

prop_applicativeId as = (pure id <*> as) == (as :: Arr Int)

prop_applicativeComp (Blind as) (Blind bs) (Blind cs) =
  (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))
  where u = as
        v = bs
        w = cs :: Arr Int

prop_applicativeHomomorphism (Blind f) a =
  (pure f <*> (pure a :: Arr Int)) == pure (f a)

prop_applicativeInterchange (Blind u) y =
  (u <*> (pure y :: Arr Int)) == (pure ($ y) <*> u)
  

return []
runTests = $quickCheckAll

main = do
  runTests
  
  let n = 1000
      as = fromList [0..n]

  defaultMain [
    bgroup "array" [
       bench "applicative" $ nf applicativeWork as
     , bench "monad"       $ nf monadWork as]]
