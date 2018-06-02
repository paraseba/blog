---
title: An exercise on applicatives
description: A small exercise in "Applicative programming with effects"
meta-title: A Haskell exercise in "Applicative programming with effects"
meta-description: "Title: An exercise on applicatives, Topic: Applicative functors, Const, Compose, Language: Haskell, Published: 2018-06-02"
---

Rereading the great Functional Pearl ["Applicative programming with effects"](http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf) I found the following:

<blockquote>
We began this section by observing that `Accy o` is not a monad. However, given
`Monoid o` it can be defined as the composition of two applicative functors derived
from monads—which two, we leave as an exercise
</blockquote>

What they call `Accy o` is what we would call today `Const o` from [`Data.Functor.Const`](https://hackage.haskell.org/package/base/docs/Data-Functor-Const.html)


> newtype Const o a = Const { getConst :: o }

a phantom type that forgets about `a` and just carries around the `o`.

`Const o` can be made an instance of `Applicative` if `o` has a `Monoid`:

> instance Functor (Const o) where
>   fmap _ (Const o) = Const o
>
> instance Monoid o => Applicative (Const o) where
>   pure _ = Const mempty
>   Const a <*> Const b = Const (a <> b)

Since we are forgetting about the `a`'s, the only way to combine two `Const o` is
to use the monoid on `o`.

`Const o` is a pretty unusual applicative. It's surprising that it satisfies all the
laws by discarding so much. Let's verify it is actually an `Applicative` by checking the laws:

First the `Functor` laws:

> -- identity
> fmap id (Const o) = Const o = id (Const o)
>
> -- composition
> fmap (f.g) (Const o) = Const o =
>   = fmap f (fmap g (Const o)) = (fmap f . fmap g) (Const o)

And now the `Applicative` laws:

> -- identity
> pure id <*> Const o = Const mempty <*> Const o =
>   = Const (mempty <> o) = Const o
>
> -- composition
> pure (.) <*> Const u <*> Const v <*> Const w =
>   = Const (mempty <> u <> v <> w) = Const (u <> v <> w) =
>   = Const u <*> (Const v <*> Const w)
>
> -- homomorphism
> pure f <*> pure x = Const mempty <*> Const mempty =
>   = Const (mempty <*> mempty) = Const mempty = pure (f x)
>
> -- interchange
> Const f <*> pure y = Const f <*> Const mempty =
>   = Const f = Const mempty <*> Const f = pure ($ y) <*> Const f


OK, back to the exercise, how can we write `Const` as the composition of two Monads? If
we achieved this, we would get the `Applicative` instance, and the proof of the laws
for free. That's because there is an instance

> (Applicative f, Applicative g) => Applicative (Compose f g)

`Const`'s applicative combines effects using a monoid. The other basic monad we know of with
that same behavior is Writer, which also combines its payload using a monoid. So, the `Const o`
applicative looks a lot like the `Writer o` applicative. We could write:

> import Control.Monad.Writer
>
> newtype Const' o a = Const' {getConst' :: (Writer o a)}
>   deriving (Functor, Applicative)

But this doesn't really ignore the `a` type. When using this code, some type and value for `a`
must be selected. Even more, the applicative will actually carry out the work of operating on
the `a`'s. What we want is some type that can ignore `a` completely.
Enter [`Proxy`](https://hackage.haskell.org/package/base/docs/Data-Proxy.html)
in the base library

> data Proxy t = Proxy

Again a phantom type monad. There is a single inhabitant in this type, `Proxy`, so the
`Applicative` doesn't do any computation at all. The functor, applicative and monad laws
for `Proxy t` are satisfied by construction, because every `Proxy t` is equal to every
"other" `Proxy t`.

Using `Proxy` we get a better candidate for the `Writer` result type:

> newtype Const' o a = Const' {getConst' :: (Writer o (Proxy a))}

or, rewriting this in terms of `Compose`[^compose]:

> newtype Const' o a =
>   Const' { getConst' :: Compose (Writer o) Proxy a }
>     deriving (Functor, Applicative)

With this version of `Const'`, we don't need to write the instance or prove the applicative laws,
and yet, now we can use `Const'` to traverse combining with the monoid.

> sum :: (Traversable t, Num n) => t n -> n
> sum =  unwrap . traverse wrap
>   where
>     wrap = Const' . Compose . writer . (Proxy,) . Sum
>     unwrap = getSum . execWriter . getCompose . getConst'


[^compose]: <sup>1</sup> As a reminder, functor composition is declared as

    ```haskell
    newtype Compose f g a = Compose { getCompose :: f (g a) }
    ```
