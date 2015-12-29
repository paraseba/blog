{-# LANGUAGE TemplateHaskell #-}

import           Control.Applicative (liftA2)
import           Data.List           (sort)
import           Data.Monoid         ((<>))
import           Test.QuickCheck

isSorted :: (Foldable t, Ord a) => t a -> Bool
isSorted = not . isFailed . foldMap mkSorted

isFailed :: Sorted a -> Bool
isFailed Failed = True
isFailed _ = False

mkSorted :: a -> Sorted a
mkSorted a = Range a a

data Sorted a =
  Init
  | Failed
  | Range a a
  deriving (Show, Eq)

instance (Ord a) => Monoid (Sorted a) where
  mempty = Init

  Failed      `mappend` _           = Failed
  _           `mappend` Failed      = Failed
  Init        `mappend` s           = s
  s           `mappend` Init        = s
  Range a1 b1 `mappend` Range a2 b2
    | a2 >= b1                      = Range a1 b2
    | otherwise                     = Failed

instance (Arbitrary a) => Arbitrary (Sorted a) where
  arbitrary = frequency [(1,return Init)
                        ,(2,return Failed)
                        ,(4,liftA2 Range arbitrary arbitrary)
                        ]

  shrink Failed = [Init]
  shrink Init = []
  shrink (Range a b) = Init : Failed : [Range a' b' | (a', b') <- shrink (a,b)]

--- Monoid laws
prop_sortedAssoc :: Sorted Int -> Sorted Int -> Sorted Int -> Bool
prop_sortedAssoc a b c = (a <> b) <> c == a <> (b <> c)

prop_sortedLeftId :: Sorted Int -> Bool
prop_sortedLeftId s = mempty <> s == s

prop_sortedRightId :: Sorted Int -> Bool
prop_sortedRightId s = s <> mempty == s


--- isSorted properties
prop_isSortedForSortedLists :: [Int] -> Bool
prop_isSortedForSortedLists = isSorted . sort

prop_isSortedIfSorted :: [Int] -> Bool
prop_isSortedIfSorted as = isSorted as == isSorted'
  where isSorted' = sort as == as

prop_canIdentifyPartialUnsorted :: [Int] -> [Int] -> Property
prop_canIdentifyPartialUnsorted any unsorted =
  not (isSorted unsorted) ==> not (isSorted partial)
    where partial = any ++ [undefined] ++ unsorted



return []
allProps= $forAllProperties

main =
  allProps (quickCheckWithResult stdArgs{maxSuccess = 5000})

