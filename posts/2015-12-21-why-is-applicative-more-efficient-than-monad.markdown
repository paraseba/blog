---
title: Why is Applicative More Efficient Than Monad
author: Sebastian Galkin
---

It is well known that Monad is more powerful than Applicative Functor. Using
the Monad methods you can implement the Applicative ones. To the point that in
recent GHC versions we have

``` haskell
class Applicative m => Monad m where

```

with laws


``` haskell
pure = return

(<*>) = ap
```

and the implementation

```haskell
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap m1 m2 = do
  x1 <- m1
  x2 <- m2
  return (x1 x2)
```

There are many good examples of Applicatives that are not, and can not be Monads,
like
[Validation](http://haddock.stackage.org/lts-3.19/either-4.4.1/Data-Either-Validation.html)
and [ZipList](http://haddock.stackage.org/base-4.8.2.0/Control-Applicative.html#v:ZipList)

But the question I asked myself is:

> If we have an Applicative that is also a Monad,
> is there any reason to prefer `<*>` over `ap`

From the Monad law above we know that in fact they produce the same result, but
could important performance differences exist?

Comparing the Monad and Applicative minimal implementations, we can expect some
kind of performance difference. After all, with `>>=` the continuation function
has to create the monadic context dynamically:

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

The function has type `Monad m => a -> m b`, so it has to create the context
during execution. On the other hand, for Applicative, the output context
is fully defined by the "program" not by the evaluation of the function:

```haskell
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```

Can we use the fact that the applicative context is known at compilation to
get a performance advantage when compared to the monad? The goal is find a
monad where using `<*>` does less work than calling `ap`.

## Developing some intuition

When we do `ma >>= f`, `f` is in charge of creating the final monadic context.
But `f :: a -> m b` doesn't have access to the original context it is being
chained to. So the new context gets created with no knowledge of the original
one.

On the other hand, when we do `f k <*> f a` the `<*>` operator itself is in
charge of creating the output applicative context, and it does so with access
to the initial one. In that way, there is opportunity for optimizing the creation
of the output context.

Based on this intuition, let's try to find an example in a monad where creating
the output context could be optimized with more knowledge.

## An array Monad

Regular Haskell Arrays, in `Data.Array` are not monads or applicatives. They are
too powerful, allowing for arbitrary index values. For example, let's take right
identity law for monads

```haskell
  as >>= return = as
```

`as` will have certain index values, but we have no way make `return` create
an array with the same index values for all `as`. There is no way to satisfy
the law.

But we can create our own, much simplified 1D array that can in fact be turned
into a Monad. Let's create the most basic array, in fact using Haskell's
`Data.Array` as a backend:


```haskell
data Arr a = Arr {toArray :: !(Array Integer a)}

fromList :: [a] -> Arr a
fromList [] = error "No empty arrays"
fromList as = Arr $ listArray (0, genericLength as - 1) as
```

We can only create these arrays from a list. Now we provide instances for
`Functor`, `Applicative` and `Monad`.

```haskell
instance Functor Arr where
  fmap f = Arr . fmap f . toArray
```
Nothing fancy there, the usual unwrapping and wrapping and delegating to
`Data.Array`'s implementation. 

For the `<*>` to behave similarly to lists, we want to apply every function
on the left to every value in the array on the right. We can use list
comprehension and the fact that `Data.Arrays` are `Foldable` so they provide
`toList`. Since we are at it, we make our `Arr` also `Foldable`

```haskell
instance Foldable Arr where
  foldMap f = foldMap f . toArray

instance Applicative Arr where
  pure =  fromList . pure
  fs <*> as = fromList [f a | f <- toList fs, a <- toList as]
```

The key here is that we only ever have to create a single `Arr`, since we know
the applicative contex on the left and right, we know exactly what the size
of the resulting array will be, and we can just construct it. This is all
very inefficient, for instance our `fromList` iterates the list more than once,
but it shows the point of not requiring more than one `Arr` construction.

On the other hand, when we make `Arr` a `Monad`


```haskell
instance Monad Arr where
  return = fromList . return
  as >>= f = fromList $ concatMap (toList . f) as
```

There is no way around it, each call to `f` creates a new `Arr`, and finally we
need to create yet another big array. `Data.Array` is strict on the indexes, this
is more work that for the `Applicative` case.

### Benchmark
Let's run the same operation using both the `Applicative` and the `Monad`

```haskell
monadWork :: Arr Int -> Int
monadWork as = sum $ do
  i <- as
  j <- as
  return (i + j)

applicativeWork :: Arr Int -> Int
applicativeWork as = sum $ (+) <$> as <*> as

main = do
  let n = 1000
      as = fromList [0..n]

  defaultMain [
    bgroup "array" [
       bench "applicative" $ nf applicativeWork as
     , bench "monad"       $ nf monadWork as]]
```

Using criterion we can verify the intuition, the Applicative is more than three
times faster than the Monad.

<img width="100%" src="../images/array-criterion.png" alt="Criterion Array Result">


## Code
You can find all the source code [here](https://github.com/paraseba)
