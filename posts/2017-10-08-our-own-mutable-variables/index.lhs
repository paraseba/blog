---
title: Our own mutable variables
description: Defining mutable variables in different monads
meta-title: Defining mutable Haskell variables in different monads
meta-description: "Title: Our own mutable variables, Topic: Mutable variables and monads, Language: Haskell, Published: 2017-10-08"
---

Talking with a friend earlier today we decided to make an experiment in
declaring generic mutable variables that can be used in `IO`, `ST` or `State`
monads. I don't think this is useful, but it was fun to write. Here is
result.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FunctionalDependencies  #-}

import Data.Array.MArray
import Data.Array.IO
import Control.Monad.ST
import Data.STRef
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import qualified Control.Monad.State.Strict as State
import Data.Foldable (forM_)
import Control.Monad (when)

import Test.QuickCheck
import Test.QuickCheck.Monadic
\end{code}

Let's define the abstract interface. `v` will be the type of variables
that can operate on the monad `m`, holding values of type `a`. We need
three operation: create a variable, read, and write to it.

\begin{code}
class Var v m a | m -> v where
  new :: a -> m (v a)
  get :: v a -> m a
  set :: v a -> a -> m ()
\end{code}

Notice we had to use `FunctionalDependencies` to ease type inference. I don't
like this, there is probably a better way.

A utility function to do both read and write passing through a function:

\begin{code}
modify :: (Monad m, Var v m a) => (a -> a) -> v a -> m ()
modify f v = get v >>= set v . f
\end{code}

Now we can provide different implementations for variables. First one in `IO`

\begin{code}
newtype IOVar a = IOVar (IOArray Int a)
\end{code}

We represent the value as an array of a single element. This is obviously
overkill, but the goal was also to experiment with the low level array API

\begin{code}
instance Var IOVar IO a where
  new = fmap IOVar . newArray (0, 0)
  get (IOVar ar) = readArray ar 0
  set (IOVar ar) a = writeArray ar 0 a
\end{code}

The implementation is straightforward, reading and writing from/to the position
0 in the array. Creation needs to take care of wrapping the array in the `IOVar`
constructor.

Providing an implementation in the `ST` monad is not much harder. Here, we could
also use an `STArray`, but we go directly to `STRef` for simplicity

\begin{code}
newtype STVar s a = STVar (STRef s a)

instance Var (STVar s) (ST s) a where
  new = fmap STVar . newSTRef
  get (STVar ref) = readSTRef ref
  set (STVar ref) a = writeSTRef ref a
\end{code}

The code looks very similar to the `IO` case.

Finally, let's try to implement a variable in the `State` monad. For a variable
holding values of type `a`, it is enough to maintain state `a`. So we can
define

\begin{code}
newtype StateVar a = StateVar a
\end{code}

And now to create an instance of `Var` we can do

\begin{code}
instance Var StateVar (State.State a) a where
  new x = State.StateT $ \_ -> return (StateVar x, x)
  get _ = State.get
  set _ = State.put
\end{code}

`get` and `set` are simple. `new` requires some care. Initializing
the variable means setting the state to a given value, so it can then
be read by `get`. So in `new` we need it ignore the current state, and
set it to `x`. The types are not enough to ensure correctness, there
is a wrong implementation that also compiles:

\begin{code}
wrongNew x = return . StateVar
\end{code}

And that's it, we have the three types of variables we wanted.
Now we can write a stateful looking algorithm, computing the maximum of a
list is a good example. The way people do this in non functional languages
usually is:

- initialize a variable `max` with the first element of the list
- go through all other elements:
    * if the current element is larger than `max`, update `max` with the new value
- when done iterating the list return `max`

We can express exactly this algorithm with our variables, even more, we can do
it in a way that is generic for every type of `Var` and every supported `Monad`

\begin{code}
myMaximum :: (Monad m, Var v m a, Ord a) => NonEmpty a -> m a
\end{code}

Take a look at the signature: given a non empty list (`NonEmpty a`), we return
its maximum in some monad (`Monad m`). We can do this
as long as `a` can be ordered (`Ord a`), and there is some type of variable `v` which works
for the monad `m` and the type `a` (`Var v m a`). The type signature expresses all this pretty well.

\begin{code}
myMaximum xs = do
  max <- new (NE.head xs) -- initialize a new var
  forM_ (NE.tail xs) $ \a -> do  -- for each el after the head
    maxSoFar <- get max  -- get the current maximum
    when (a > maxSoFar) $  -- compare with current element
      set max a  -- update if needed
  get max
\end{code}

Just like in the description of the algorithm, we create a variable and update
it for every element that is larger than the initial value. When done iterating
we return the last value hold by the variable.

Now we need to write some tests:

In `IO`

\begin{code}
testIO :: (NonEmptyList Int) -> Property
testIO (NonEmpty xs) = monadicIO $ do
  mine <- run . myMaximum . NE.fromList $ xs
  assert $ mine == maximum xs
\end{code}

In `ST`

\begin{code}
testST :: (NonEmptyList Int) -> Property
testST (NonEmpty xs) = monadicST $ do
  mine <- run . myMaximum . NE.fromList $ xs
  assert $ mine == maximum xs
\end{code}

And in `State`

\begin{code}
testState :: (NonEmptyList Int) -> Bool
testState (NonEmpty xs) =
  State.execState (mine xs) whoCares == maximum xs
  where
    mine = myMaximum . NE.fromList
    whoCares = 42
\end{code}

Running the QuickCheck tests

\begin{code}
main = do
  quickCheckWith opts testIO
  quickCheckWith opts testST
  quickCheckWith opts testState
  where opts = stdArgs {maxSuccess = 5000}
\end{code}

Success!

```bash
+++ OK, passed 5000 tests.
+++ OK, passed 5000 tests.
+++ OK, passed 5000 tests.
```
