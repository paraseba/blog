import Control.Applicative (Const(..))
import Control.Monad.Trans.State
import Data.Bifunctor
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity
import Data.Functor.Product (Product(..))
import Data.Traversable

contentsBody :: a -> Const [a] b
contentsBody = Const . (: [])

contents
  :: Traversable t
  => t a -> [a]
contents = getConst . traverse contentsBody

shapeBody :: a -> Identity ()
shapeBody _ = Identity ()

shape
  :: Traversable t
  => t a -> t ()
shape = runIdentity . traverse shapeBody

prod :: (a -> m b) -> (a -> n b) -> (a -> Product m n b)
prod f g a = Pair (f a) (g a)


reassembleBody :: () -> Compose (State [a]) Maybe a
reassembleBody _ = Compose (state takeHead)
  where
    takeHead (a:as) = (Just a, as)
    takeHead [] = (Nothing, [])

reassemble
  :: Traversable t
  => t () -> Compose (State [a]) Maybe (t a)
reassemble = traverse reassembleBody

reconstruct
  :: Traversable t
  => t () -> [a] -> Maybe (t a)
reconstruct = evalState . getCompose . reassemble

-- more efficient in single pass
decompose
  :: Traversable t
  => t a -> Product (Const [a]) Identity (t ())
decompose = traverse (prod contentsBody shapeBody)

swap
  :: (Traversable s, Traversable t)
  => t a -> s b -> (Maybe (t b), Maybe (s a))
swap x y = (reconstruct xShape yData, reconstruct yShape xData)
  where
    Pair (Const xData) (Identity xShape) = decompose x
    Pair (Const yData) (Identity yShape) = decompose y


data Tree a
  = Empty
  | Leaf a
  | Node (Tree a)
         a
         (Tree a)
  deriving (Show)

instance Functor Tree where
  fmap = fmapDefault

instance Foldable Tree where
  foldMap = foldMapDefault

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l k r) =
    Node <$> traverse f l <*> f k <*> traverse f r

main :: IO ()
main = do
  print $ contents [1 .. 5]
  print $ shape [1 .. 5]
  print $ runState (getCompose $ reassemble (shape [1 .. 5])) [10 .. 23]
  print $ reconstruct [(), ()] [1, 2]
  print (shape [1, 2, 3])
  print (contents [1, 2, 3])
  print $ contents [[], [1], [5]]
  print $ swap [1, 2, 3] [[], [1], [5]]
  print $ swap [2, 3] [[], [1], [5]]
  let t = Node left 3 right
  print $ swap t ['a'..'f']
  print $ swap t ['a'..'z']

    where left = Node (Leaf 1) 2 Empty
          right = Node (Leaf 4) 5 (Leaf 6)
