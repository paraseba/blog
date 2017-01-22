---
title: Beautiful Power Series
description: A tiny Haskell formulation for power series
meta-title: A Haskell formultation for power series
meta-description: "Title: Beautiful Power Series, Topic: Haskell lazyness math, Language: Haskell, Published: 2015-12-26"
---

I'm getting better at Haskell, or at least that's what I choose to believe. Anyway,
I recently joined Haskell-cafe, one of the e-mail distribution lists, and I found
this great
[thread](https://mail.haskell.org/pipermail/haskell-cafe/2015-December/122521.html)
where Kim-Ee Yeoh links to a **gorgeous**
article *[Power serious: power series in ten one-liners](http://www.cs.dartmouth.edu/~doug/powser.html)*.

In the article Doug McIlroy, in a few one-liners, defines infinite power series
for trigonometric functions exploiting the power of Haskell's lazy evaluation.

As a teaser, this is the code for the `sin` and `cos` series[^1],

```haskell
sins = int coss
coss = 1 - int sins
```

How awesome is that!

I have to find some time to play with the code. It makes me happy that this can
be written so simply and beautifully, we must be doing something right.

[^1]: <sup>1</sup> `int` is integration and it can also be trivially defined.
