---
title: Shape and contents with traversables
description: An interesting idea found in a paper, splitting a collection in its elements and shape
meta-title: Splitting a collection in its elements and shape using traversals
meta-description: "Title: Shape and contents with traversables, Topic: Traversable Applicative Product Composition, Language: Haskell, Published: 2016-01-21"
---

One of the first papers I could find that seriously studies the properties of Traversals is
**"The essence of the iterator pattern"**, by _Jeremy Gibbons_ and _Bruno C. d. S. Oliveira_ [(PDF)](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf)

In there, they show the idea of splitting a traversable collection in its contents and its shape, attributing this idea to Moggi _et al._ in 
_"Monads, Shapely Functors and Traversals"_. The idea is to traverse the collection extracting shape and elements in a way that would allow to reconstruct the original structure.

To represent the contents of a traversable collections `Traversable t => t a` we can use simply `[a]`. For the shape, we need to conserve the traversable structure, discarding the elements: `Traversable t => t ()`.

## Extracting contents

Let's start with the contents. We want a function of type

```haskell
Traversable t => t a -> [a]
```

The type class function `traverse` can do the job of iterating over all the elements giving us access to each of them, we just need to provide the right `Applicative f`:

```haskell
traverse
  :: (Traversable t, Applicative f)
  => (a -> f b) -> t a -> f (t b)

```

What we want is to accumulate each element on a list, monoid style. Fortunately, every monoid, and lists in particular, can generate an applicative that uses the monoid operation to combine effects in `<*>`, and `mempty` for `pure`. Haskell calls this monoid `Const` apparently, because it looks like the `const` function, it just ignores the second argument:

```haskell
newtype Const a b = Const { getConst :: a }

instance Functor (Const m) where
    fmap _ (Const v) = Const v

instance Monoid m => Applicative (Const m) where
    pure _ = Const mempty
    Const f <*> Const v = Const (f `mappend` v)
```

So this `Const` applicative behaves like the monoid in its first argument

```haskell
$> Const [5] <*> Const [1]
Const [5,1]
```
and it's exactly what we need to implement our `contents` function:

```haskell
contentsBody :: a -> Const [a] b
contentsBody = Const . (: [])

contents
  :: Traversable t
  => t a -> [a]
contents = getConst . traverse contentsBody

$> contents (Just 42)
[42]

$> contents Nothing
[]
```

To make the examples more interesting, let's define a `Tree` type

```haskell
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
```

and try `contents` on it

```haskell
$> t = Node left 3 right
     where left = Node (Leaf 1) 2 Empty
           right = Node (Leaf 4) 5 (Leaf 6)

$> elems = contents t
[1,2,3,4,5,6]
```

We will continue to use this tree `t` in future examples.

## Extracting shape

To extract the shape of the collection we want to `traverse` it ignoring all elements. The right applicative to do that is `Identity` found in `Data.Functor` in the [transformers](https://hackage.haskell.org/package/transformers) package, or in a modern enough ghc base (>= 4.8.0.0)

```haskell
newtype Identity a = Identity { runIdentity :: a }

instance Applicative Identity where
    pure  = Identity
    Identity f (<*>) Identity a = Identity (f a)

```

This applicative basically "does nothing", which is what we want to extract the shape, no effects. Using this applicative and `traverse` we can write

```haskell
shapeBody :: a -> Identity ()
shapeBody _ = Identity ()

shape
  :: Traversable t
  => t a -> t ()
shape = runIdentity . traverse shapeBody

$> shape t
Node (Node (Leaf ()) () Empty) () (Node (Leaf ()) () (Leaf ()))
```

## Contents and shape in one pass

If we want to compute both the contents _and_ the shape, we can call `traverse` twice, but there is a better way. The product of two applicatives is guaranteed to be an applicative, unlike for instance the product of two monads. That means that we can write in a generic way the applicative instance for an arbitrary pair of applicatives. The `base` package already has this `Product` type in `Data.Functor.Product`

```haskell
data Product f g a = Pair (f a) (g a)

instance (Applicative f, Applicative g) =>
         Applicative (Product f g) where
  pure x = Pair (pure x) (pure x)
  Pair f g <*> Pair x y = Pair (f <*> x) (g <*> y)
```

As we can see, this applicative tracks the effects of `f` and `g` in parallel, using a tuple-like `Pair` constructor.

With this, and in a single traversal we can compute both the contents and the shape:

```haskell
prod :: (a -> m b) -> (a -> n b) -> (a -> Product m n b)
prod f g a = Pair (f a) (g a)

decompose
  :: Traversable t
  => t a -> Product (Const [a]) Identity (t ())
decompose = traverse (prod contentsBody shapeBody)

$> decompose t
Pair
  (Const [1, 2, 3, 4, 5, 6])
  (Identity (Node (Node (Leaf ()) () Empty)
                  ()
                  (Node (Leaf ()) () (Leaf ()))))
```

## Reconstructing

Now the paper proposes to reconstruct the original traversable from it's shape and contents as extracted in the previous sections. This sounds like a fold, but we can also think about it as a stateful computation. The state being tracked is the list of elements, the contents. For each element of the desired shape, we extract the first element from the state and leave the rest in the new state. Since every monad is an applicative we know we'll be able to use the `State` monad in a call to `traverse`.

But there is a one extra detail to take into account. If the number of elements provided as content are not enough to fill the shape, we won't be able to recreate the datastructure. For this reason the end result has to be optional. So we have a combination of a State applicative with a `Maybe`, in a composition of both effects.

Just like in the case of `Product` the composition of two applicatives is also an applicative

```haskell
newtype Compose f g a = Compose { getCompose :: f (g a) }

instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
  pure x = Compose (pure (pure x))
  Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)
```

In this form, `Compose (State [a]) Maybe` gives us the exact combination of effects we want. We can write the function to reassemble now


```haskell
reassemble
  :: Traversable t
  => t () -> Compose (State [a]) Maybe (t a)
reassemble = traverse reassembleBody
```

This `reassembleBody` function must take a `()` and return the composed stateful/optional computation

```haskell
reassembleBody :: () -> Compose (State [a]) Maybe a
reassembleBody _ = Compose (state takeHead)
  where
    takeHead (a:as) = (Just a, as)
    takeHead [] = (Nothing, [])
```

Now to reconstruct the datastructure we just need to feed the shape to `reassemble` and then run the stateful computation resulting

```haskell
reconstruct
  :: Traversable t
  => t () -> [a] -> Maybe (t a)
reconstruct = evalState . getCompose . reassemble
```

Here, we are discarding any extra elements provided.

## Swapping data

With this machinery we can, for instance, write a generic way to _swap_ the contents of two datastructures of differente shapes.

```haskell
swap
  :: (Traversable s, Traversable t)
  => t a -> s b -> (Maybe (t b), Maybe (s a))
swap x y = (reconstruct xShape yData, reconstruct yShape xData)
  where
    Pair (Const xData) (Identity xShape) = decompose x
    Pair (Const yData) (Identity yShape) = decompose y
    

$> swap t ['a'..'f']
( Just (Node (Node (Leaf 'a') 'b' Empty)
             'c'
             (Node (Leaf 'd') 'e' (Leaf 'f')))
, Just [1, 2, 3, 4, 5, 6])

$> swap t ['a'..'z']
( Just (Node (Node (Leaf 'a') 'b' Empty)
             'c'
             (Node (Leaf 'd') 'e' (Leaf 'f')))
, Nothing)
```

## Final notes

It's a great [paper](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf), I highly recommend to read it. This shape/contents thing is only a short section in the paper, it goes in several other directions with many other interesting ideas.

If you are an intermediate level Haskell programmer, reading classic papers is great, particularly old ones (this on is from 2009, so not that old). Lots of ideas, plainly explained.
