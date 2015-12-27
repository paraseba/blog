---
title: Why is Applicative More Efficient Than Monad
description: One reason to use Applicative instead of Monad
---

It is well known that `Monad` is more powerful than `Applicative` Functor. Using
the Monad methods you can implement the Applicative ones, to the point that in
recent GHC versions we have

``` haskell
class Applicative m => Monad m

```

with equivalence laws


``` haskell
pure = return

(<*>) = ap
```

and default implementation

```haskell
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap m1 m2 = do
  x1 <- m1
  x2 <- m2
  return (x1 x2)
```

There are many good examples of Applicatives that are not, and can not be, `Monads`,
like
[Validation](http://haddock.stackage.org/lts-3.19/either-4.4.1/Data-Either-Validation.html)
and [ZipList](http://haddock.stackage.org/lts-3.19/base-4.8.1.0/Control-Applicative.html#v:ZipList)

But the question I was asking myself these days:

> If we have an Applicative that is also a Monad,
> is there any reason to prefer `<*>` over `ap`

## Developing some intuition

From the Monad laws above we know that in fact they produce the same result, but
could important performance differences exist?

Comparing the `Monad` and `Applicative` minimal implementations, we can expect some
kind of performance difference. After all, with `>>=` the continuation function
has to create the monadic context dynamically:

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

The right argument to `>>=` has type `Monad m => a -> m b`, so it has to
create the monadic context
during execution. On the other hand, for `Applicative`, the output context
is fully defined by the "program" not by the evaluation of the function:

```haskell
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```

When we do `ma >>= f`, `f` is in charge of creating the final monadic context.
But `f :: a -> m b` doesn't have access to the original context it is being
chained to. So the new context gets created with no knowledge of the original
one.

On the other hand, when we do `f k <*> f a` the `<*>` operator itself is in
charge of creating the output applicative context, and it does so with access
to the initial one. In that way, there is opportunity for optimizing the creation
of the output context.

Based on this intuition, let's try to find an example in a monad where creating
the output context could be optimized with the extra knowledge.

## An array Monad

Regular Haskell Arrays, in `Data.Array` are not monads or applicatives. They are
too powerful, allowing for arbitrary index values. For example, let's take the right
identity law for monads

```haskell
  as >>= return = as
```

`as` will have certain index values, but we have no way to make `return` create
an array with the same index values `as` has, for all `as`. There is no way to satisfy
the law.

But we can create our own, much simplified, 1D array that can in fact be turned
into a Monad. Let's write the most basic array, in fact using Haskell's
`Data.Array` as a backend:


```haskell
data Arr a = Arr {toArray :: !(Array Integer a)}

fromList :: [a] -> Arr a
fromList [] = error "No empty arrays"
fromList as = Arr $ listArray (0, genericLength as - 1) as
```

We can only create these arrays from a list. These are terrible arrays,
performance is going to be awful, but that's not the point.

Now we provide instances for `Functor`, `Applicative` and `Monad`.

```haskell
instance Functor Arr where
  fmap f = Arr . fmap f . toArray
```
Nothing fancy there, the usual unwrapping and wrapping and delegating to
`Data.Array`'s implementation. 

For the `<*>` to behave similarly to lists, we want to apply every function
on the left to every value in the right argument array. We can use list
comprehension and the fact that `Data.Arrays` are `Foldable` so they provide
`toList`. Since we are at it, we make our `Arr` also `Foldable`

```haskell
instance Foldable Arr where
  foldMap f = foldMap f . toArray

instance Applicative Arr where
  pure =  fromList . pure
  fs <*> as = fromList [f a | f <- toList fs, a <- toList as]
```

Again, this is going to be horrible performance, we turn the arrays into lists,
then use the lists for the cartesian product, and finally turn the resulting
back into an `Arr`.
The key here is that we only create a single `Arr`, since we know
the applicative contexts on the left and right, we know exactly what the size
of the resulting array will be, and we can just construct it.

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
Let's run the same operation using both the `Applicative` and the `Monad`. The
fantastic [criterion](http://haddock.stackage.org/lts-3.19/criterion-1.1.0.0/index.html)
library can be used to get some numbers.

```haskell
applicativeWork :: (Applicative f, Foldable f, Num a) =>
  f a -> a
applicativeWork as = sum $ (+) <$> as <*> as

monadWork :: (Monad f, Foldable f, Num a) =>
  f a -> a
monadWork as = sum $ ((+) <$> as) `ap` as
```

We sum the results of a cartesian product, both on the applicative context,
using `<*>`, and on the monadic context using `ap`. And now we drive it with
criterion's `defaultMain` and a reasonable size. As a control case, we do the
same for lists.

```haskell
main = do
  let n  = 500 :: Int
      l  = [0..n]
      as = fromList l

  defaultMain [
    bgroup "array" [
       bench "applicative" $ nf applicativeWork as
     , bench "monad"       $ nf monadWork as]

   ,bgroup "list" [
       bench "applicative" $ nf applicativeWork l
     , bench "monad"       $ nf monadWork l]
    ]
```

<img width="100%" src="../images/array-criterion.png" alt="Criterion Array Result"/>

We see for `Arr` the `Applicative` is around three times faster than the `Monad`,
while for lists, times are exactly the same.

So, that's one reason to prefer `<*>` over `ap`, for some monads the former can
be vastly more efficient.


## Code
You can find all the source for this post on
[GitHub](https://github.com/paraseba/blog/blob/master/code/why-is-applicative-more-efficient-than-monad/Main.hs)
