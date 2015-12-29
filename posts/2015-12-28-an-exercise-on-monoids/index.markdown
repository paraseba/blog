---
title: An exercise using Monoids
description: A simple but interesting Haskell problem using Monoids and Foldable
---

I found a fun exercise in
*["Functional Programming in Scala"](https://www.manning.com/books/functional-programming-in-scala)*,
a book I'm [reading](/posts/diverse-readings) these days. This is the exercise
description, slightly generalized and translated to Haskell types:

> Use a `Monoid` and `foldMap` to detect if a given `Foldable` is ordered.

Let's start by thinking what the type of the requested function is. The argument
is a `Foldable`, so we will need `(Foldable t) =>`. Since we want to check for
ordering, we will need to compare elements in the datastructure, so we will also
need `(Ord a) =>`. The result will of course be a `Bool` indicating if the
data is sorted. Putting it all together, this is the type of the function we
want:

```haskell
isSorted :: (Foldable t, Ord a) => t a -> Bool
```

## Specifying the function with `QuickCheck`
Let's now write down a couple of [QuickCheck](https://www.stackage.org/package/QuickCheck)
properties for the `isSorted` function:

- `isSorted` should be true for sorted lists
    ```haskell
    prop_isSortedForSortedLists :: [Int] -> Bool
    prop_isSortedForSortedLists = isSorted . sort
    ```
  if we first sort the list, then `isSorted` must return `True`.

- How about unsorted lists? A simple strategy we can use is to compare the
  output of `isSorted` to the output of a much simple implementation of the same
  function. The simplest way I kind think of to know if a list is sorted, is to
  actually sort it and verify that the result is equal to the original.
    ```haskell
     prop_isSortedIfSorted :: [Int] -> Bool
     prop_isSortedIfSorted as = isSorted as == isSorted'
       where isSorted' = sort as == as
    ```

The first property is redundant given the second one, but we keep it to make sure
we test `isSorted` with enough sorted lists. Finally, `sort as == as` is not
necessarily equivalent to `isSorted` unless `sort` is stable, but
Haskell's list's `sort` is in fact stable, and we are good to go.


## Developing intuition
The exercise asks us to use [`foldMap`](http://haddock.stackage.org/nightly-2015-12-29/base-4.8.2.0/Prelude.html#v:foldMap)

```haskell
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
```
Using `foldMap` we have the opportunity to transform every element in the data
structure by turning it into some `Monoid` and then use `mappend` between pairs,
starting with `mempty`. For a list, the end result looks something like:

```haskell
foldMap f [a,b,c] = f a <> f b <> f c <> mempty
```
`<>` is simply an infix synonym for `mappend`, and we don't need parentheses
because `mappend` is associative.

The key is to find a `Monoid` that can keep track of the elements it has seen,
and make sure the next one is in the right order.

My first intuition was to use some kind of wrapper over `Maybe a`.
`Nothing` would represent an unsorted element detected, and `Just` would wrap the
right argument to `mappend`. Something like:

```haskell
newtype Sorted a = S (Maybe a)

instance (Ord a) => Monoid (Sorted a) where
  mempty = S Nothing

  S Nothing `mappend` _ = S Nothing

  _ `mappend` S Nothing = S Nothing

  S (Just a) `mappend` S (Just b)
    | b >= a = S (Just b)
    | otherwise = S Nothing
```
It turns out this simple approach has, at least, two problems:

1. `mempty` has the same representation as an unsorted element detection. This
means, for instance, that an empty list would be marked as not sorted.

2. A more significant problem is that this version of `Sorted` is not even a
  `Monoid` because it doesn't satisfy the [associativity law](https://en.wikipedia.org/wiki/Associative_property).
  Let's see a counterexample:
    ```haskell
    (S (Just 1) <> S (Just 0)) <> S (Just 1)
    = S Nothing <> S (Just 1)
    = S Nothing
    ```
    but associating to the right we get:
    ```haskell
    S (Just 1) <> (S (Just 0)) <> S (Just 1)
    = S (Just 1) <> S (Just 1)
    = S (Just 1)
    ```
Those two results should be equal to have a valid `Monoid`

To fix those problems we need to track more state. Problem 1 requires us to
track more state, in particular a way to differentiate `mempty` from ordering
failure. To solve problem 2, maintaining the largest/smallest element is not
enough, it introduces associativity problems.

## A solution
We will need a type that can distinguish the "nothing is known" case from
"ordering failed". Let's start with that:

```haskell
data Sorted a = Init | Failed
```

`Init` is the initial, know-nothing state[^1].
As we mentioned in the previous section, tracking failure and max element is not
associative. What we can do instead is to track the full interval as known so far.
In this case `mappend` can expand the interval with each new sorted element,
or fail if the new element lies within the previous interval.

Expanding our type to this we get:

```haskell
data Sorted a = Init | Failed | Range a a
```
We will need a way to initialize a `Sorted` with a single element, but that's
easy, we can create the `Range` with the element as both start and end of the
interval.

### The `Monoid` instance
Let's write the
[`Monoid`](http://haddock.stackage.org/nightly-2015-12-29/base-4.8.2.0/Prelude.html#t:Monoid)
for this type

~~~~ {.haskell .numberLines}
instance (Ord a) => Monoid (Sorted a) where

  -- we start knowing nothing
  mempty = Init

  -- failure propagates contagiously
  Failed      `mappend` _           = Failed
  _           `mappend` Failed      = Failed

  -- we maintain any information we gain
  Init        `mappend` s           = s
  s           `mappend` Init        = s

  -- this is where the detection happens
  Range a1 b1 `mappend` Range a2 b2
    | a2 >= b1                      = Range a1 b2
    | otherwise                     = Failed
~~~~

If we are *mappending* over a failure, there is nothing to do, we return the failure.
Mappending with `Init`, returns the new element. In the interesting case, mappending
`Ranges`, we verify if the new range is outside of the interval, and return the
new expanded interval, or, if the new element intersects the interval, we fail.

Is this a `Monoid` now? Let's see

- `mempty <> s = s <> mempty = s` is trivially true given the code on lines
  11 and 12.
- `Failed` on both sides of `<>` returns `Failed`, that guarantees associativity
  when there is a `Failed` in the equation.
- When there is `Init <> s` or `s <> Init` we can replace it for `s`, so we
  turn the three terms into 2 and associativity holds.
- When we have three `Ranges`
    - If ranges are properly ordered, each `<>` will expand the range
    ```haskell
    (Range a1 b1 <> Range a2 b2) <> Range a3 b3
    = Range a1 b2 <> Range a3 b3
    = Range a1 b3
    and
    Range a1 b1 <> (Range a2 b2) <> Range a3 b3)
    = Range a1 b1 <> Range a2 b3
    = Range a1 b3
    ```
    - The case with one or two failing pairs can also be proved easily, left
    as an exercise.

### The `isSorted` function
Now that we have our `Monoid` writing `isSorted` is easy. We need to map over
the `Foldable` creating `Sorted` values with empty ranges. Then reduce with
`mappend`, and finally verify that we don't end up with a `Failed`:

```haskell
isSorted :: (Foldable t, Ord a) => t a -> Bool
isSorted = not . isFailed . foldMap mkSorted

isFailed :: Sorted a -> Bool
isFailed Failed = True
isFailed _ = False

mkSorted :: a -> Sorted a
mkSorted a = Range a a
```
This code passes our specification, we are done.

## A sidenote on lazyness
Our code has an interesting property, it can detect non-ordering in partial
datastructures. That is, datastructures where *bottom* is present as an element.
Let's use the `Foldable` for lists to show this:

```haskell
isSorted [1, undefined, 2, 1, 3] = False
```

This is nice, and we got it for free. `isSorted` only inspects the insides of
the datastructure as much as it needs to make a decision. Since for lists
`foldMap` is implemented in terms of `foldr`, we need to "provide evidence"
that the list is unsorted to the right of the `undefined` element.

Let's see how the evaluation proceeds

```haskell
isSorted [0, undefined, 2, 1]

-- substituting isSorted definition
= not . isFailed . foldMap mkSorted) $ [0, undefined, 2, 1]

-- substituting foldMap definition
= not . isFailed . foldr (mappend . mkSorted) mempty $ [...]

-- substituting foldr definition and defining
-- ru = Range undefined undefined;
-- r1 = Range 1 1; r2 = Range 2 2
= not . isFailed $ r1 <> ru <> r2 <> r1 <> Init
= not . isFailed $ r1 <> ru <> r2 <> r1
= not . isFailed $ r1 <> ru <> Failed
```

At this point we notice that our `<>` implementation doesn't evaluate its left
`Range` argument [^2] when the right argument
is `Failed`. So, even in the presence of a `Range undefined undefined`, evaluation
can continue as:

```haskell
= not . isFailed $ r1 <> ru <> Failed
= not . isFailed $ r1 <> Failed
= not . isFailed $ Failed
= not True
= False
```

## Code
The complete code for the exercise and tests is on
[GitHub](https://github.com/paraseba/blog/)

[^1]: <sup>1</sup> `Init` is not essential to the problem, it's an artifact of
having to use a `Monoid`, which requires `mempty`. An alternative way would be
to replace the `Monoid` with a `Semigroup` and use `foldr1` instead of `foldMap`.

[^2]: <sup>2</sup> The first pattern match in our `mappend` implementation is
```haskell
Failed `mappend` _ = Failed
```
So, in fact, `<>` will evaluate the left argument, but only to
[Weak Head Normal Form](https://wiki.haskell.org/Weak_head_normal_form), that is,
only enough to know it's not a `Failed`, it won't touch the `undefined`.
