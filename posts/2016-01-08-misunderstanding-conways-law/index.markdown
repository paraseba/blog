---
published: 2016-01-09
title: Misunderstanding Conway's law
description: Some common mistakes engineering teams make applying Conway's law
meta-title: Common mistakes applying Conway's law in software development
meta-description: "Title: Common mistakes applying Conway's law in software development, Topic: Software development, Conway's law, Published: 2016-01-08"
---

> Any organization that designs a system (defined broadly) will produce a design
> whose structure is a copy of the organization's communication structure.

That statement is what people in the software industry call the
[Conway's law](https://en.wikipedia.org/wiki/Conway%27s_law):

I like the less precise but funnier rendering by Eric S. Raymond:

> If you have four groups working on a compiler, you'll get a 4-pass compiler.

I don't want to discuss the validity of this "law", the Wikipedia article seems
to point to some supporting evidence. My point in this post is not about the validity
but about the interpretation of Conway's law.

## What Conway's law says
Conway's law is an impossibility result. It tell us *there is no way* to have
an architecture that doesn't reflect the organization's structure.

In particular, Conway's law doesn't provide any kind of strategy to find a good
architecture. If anything, if Conway's law is a true statement, it can help
avoid time wasted trying to maintain a disagreement between organization
structure and architecture.

## What Conway's law doesn't say
Many software companies, particularly after they reach certain size, tacitly use
a different statement, calling it Conway's law:

> The proper architecture is the one that better reflects the organization's
> communication structure

1. This new statement is *absolutely* not the Conway's law. It has little
   relation to it.
2. This new statement is neither proven nor obviously true, and in fact it's
   pretty arguable.
3. This new statement is a (poor) strategy to define an architecture.

Companies sometimes use their existing communication structure to *justify*
architectural decision. This could be right or wrong, depending on the
circumstances, but Conway's law provides *no support* for this justification.

## Common patterns and mistakes
- Decisions made invoking Conway's law usually ignore the problem being solved
  completely. "We should use architecture `A` because we have organization
  structure `S`" is not a statement about software.
- Making organizational decisions without taking into account the architecture is
  always a mistake. And a pretty common one.
- Companies solving vastly different problems are usually better served by different
  architectures. This means they should probably have different communication
  structures. And yet, copying organizational models is very common in the industry.
- When the problem being solved or the solution implemented change significantly,
  a change in organizational structure should be expected.
