---
author: Rebecca Skinner
date: 2021-06-09
title: The Fixed Point
categories: ["haskell", "beginner", "programming"]
header-includes: |
  \usepackage{amsfonts}
description: "
Haskell offers ample opportunities for ah ha! moments, where figuring out just
how some function or feature works can unlock a whole new way of thinking about
how you write programs. One great example of an ah-ha moment comes from when you
can first start to understand fixed points, why you might want to use them, and
how exactly they work in haskell. In this post, you'll work through the fixed
point function in haskell, building several examples along the way. At the end
of the post you'll come away with a deeper understanding of recursion and how
haskell's lazy evaluation changes the way you can think about writing programs.
"
---

## Getting To The (Fixed) Point

Haskell offers ample opportunities for ah ha! moments, where figuring out just
how some function or feature works can unlock a whole new way of thinking about
how you write programs. One great example of an ah-ha moment comes from when you
can first start to understand fixed points, why you might want to use them, and
how exactly they work in haskell. In this post, you'll work through the fixed
point function in haskell, building several examples along the way. At the end
of the post you'll come away with a deeper understanding of recursion and how
haskell's lazy evaluation changes the way you can think about writing programs.

If you already have some experience with haskell, you may want to
[skip the first section and jump directly into learning about fix](#part2)

## A Quick Look at Recursion in Haskell

A recursive function is a function that refers back to itself. In functional
programming in general, and haskell in particular, recursion is common enough
that we've developed several different names for it, each referring to some
particular flavor or nuance of the way that the recursion is used. A couple of
terms that will we'll use as we're working toward an
understanding of fixed points in haskell are: _manual recursion_, _automatic
recursion_, and _recursive bindings_. The terms In this section we'll spend a
bit of time looking at each of these, and building up some examples, so that we
have a common framework for how we think about recursion, and a common language
for understanding the finer points of how fixed points work.

### Manual Recursion

We'll start our investigation of recursion by thinking about _manual
recursion_. When you are first learning haskell, manual recursion is probably
the thing you think about when you think about the word recursion. We call it
manual recursion because it occurs when you, the programmer, directly make a
recursive call back to the function you are currently writing.

Let's look at a classic example of recursion written in a directly recursive
style. We'll start by writing a `factorial` function. If you're not familiar
with `factorial`, it's a function that when given 0, returns 1. When given a
number greater than 0, `n`, it gives you the result of multiplying all of the
numbers from `1` to `n`. For example:

```
factorial 5 = 1 * 2 * 3 * 4 * 5 = 120
```

To make things a bit easier on ourselves in the next step, let's think about the
factorial function as _counting down_ from our input number, instead of
_counting up_ toward it, so we can say:

```
factorial 5 = 5 * 4 * 3 * 2 * 1 = 120
```

The first thing we need to do is to think about how we can take our factorial
function and break it down into increasingly smaller pieces that have the same
_shape_ as our overall function. Whenever we're writing a recrusive function, it
helps to start by looking at how we can reframe the problem in terms of
something getting smaller.

One way to see how we can break `factorial` down into smaller pieces is to
notice that:

```
factorial 5 = 5 * 4 * 3 * 2 * 1
factorial 4 =     4 * 3 * 2 * 1
```

So we can rewrite `factorial 5` to say:

```
factorial 5 = 5 * (4 * 3 * 2 * 1)
            = 5 * (factorial 4)
```

If we go one more step, we can see that:

```
factorial 4 = 4 * (3 * 2 * 1)
factorial 3 =     (2 * 2 * 1)

factorial 4 = 3 * (factorial 3)
```

From these examples you can start to see the shape of the recursive function
that we'll be writing, and how the sub-problem that we are solving at each step
gets a little bit smaller.

The next thing we want to think about before we write a recursive function is
when we should stop. This is called the _base case_. For our factorial function,
the base case is the smallest number that we can calculate a factorial for,
which is 0 and that's given to us by the definition of factorial.

With that information in hand, we can write our factorial function using direct
recursion.:

```haskell
factorial :: Integer -> Integer
factorial n =
  case n of
    0 -> 1
    n -> n * (factorial (n - 1))
```

### Automatic Recursion

Unlike manual recursion, where we can see the recursive structure of our
function by looking for the place in our code where a function calls itself, a
function that is using _automatic recursion_ does so indirectly, using a
function that manages the recursion for us automatically.

One example of automatic recursion that you're likely familiar with are the
fold functions: `foldl` and `foldr`. These two functions, and others like them,
allow you to work on data that can naturally be traversed recursively while only
having to implement the code to deal with the current element and any state that
you want to carry over across calls.

We can use a function like `foldr` to write a `factorial` by letting it do the
recursion for us:

```haskell
factorial :: Integer -> Integer
factorial n =
  let
    handleStep currentNum currentFact =
      currentNum * currentFact
  in foldr handleStep 1 [1..n]
```

Even if you've used `foldr` before, it will be helpful as we're framing the
problem to build a version of it ourselves, so that we can think through how
these sorts of recursive functions work.

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f accum items =
  case items of
    [] ->
      accum
    (next:rest) ->
      f next (foldr f accum rest)
```

Looking at this function, you can see how the shape of the function, with the
`case` statement, the base case with an empty list, and the recursive call that
each time moves one element forward in the list. Our implementation of `foldr`
is more generic to be sure- we've replaced the knowledge that `factorial 0` is
`1` with a more general statement that the value of our fold at our base case is
the initial accumulator value that was provided, and now instead of doing
multiplication directly in our recursive call we hand off the details to a
function that is passed in, but if you squint a bit you can see how similar the
two functions really are.

Using functions like folds that deal with the shape of the data and handle the
recursion for us has a number of benefits. First, it removes some unnecessary
duplication of code. Traversing data structures and doing something on all of
the elements is quite common in functional programming, and if we were to
implement it from scratch each time it would take us much longer to write
programs, and there are many more opportunities for errors. Second, it makes our
code a bit more readable by letting us center the “business logic” of our
function. In most cases, the fact that our data is represented as a list, a
binary tree, etc. is incidental to the problem at hand. By separating out the
logic for dealing with individual elements from the logic for traversing data
structures we center the relevant bits of our code. Finally, and perhaps most
importantly, functions like folds give us a common language for talking abou
the structure of our programs. For someone who has been programming for some
time, saying that something is “simply a fold over some data” can convey a good
deal of information about the general idea of how a program is implemented
without the need to bog them down in too many extraneous details.

### Recursive Let Bindings

The final type of recursion we'll look at in this first section is not so much a
specific technique to do recursion as it is a feature of haskell that allows you
to use manual recursion more easily: _recursive let bindings_.

Haskell's recursive let bindings mean that you can use recursion inside of a
`let` expression in your function. A simple example of this would be, continuing
with our factorial example, a function that computers the _double factorial_,
that is to say, the factorial of the factorial of an input number:

```haskell
-- Note: This function grows very quickly.
--   doubleFactorial 5 is a 199-digit number
--   doubleFactorial 8 is a 168187-digit number
doubleFactorial :: Integer -> Integer
doubleFactorial n =
  let
    factorial a =
      case a of
        0 -> 1
        a -> a * factorial (a - 1)
  in factorial (factorial n)
```

## <a id="part2">Fixing Functions</a>
