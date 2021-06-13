---
author: Rebecca Skinner
date: 2021-06-09
title: The Fixed Point
categories: ["haskell", "beginner", "programming"]
header-includes: |
  \usepackage{amsfonts}
summary: Learn about laziness, recursion, and fixed points in this beginner level haskell article.
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

Update: 2021-06-13: [A reader has submitted a PR to fix a few typos.](https://github.com/rebeccaskinner/rebeccaskinner.github.io/commit/496f1d572d7d7c5ab7b78f572bf7a8623862bcb7)

## A Quick Look at Recursion in Haskell

A recursive function is a function that refers back to itself. There are
different ways that you can accomplish recursion, and throughout this article
we'll look at several of them. We'll start by defining a couple of new terms to
help us differentiate some particular aspects of recursion that will matter as
we're exploring fixed points: _manual recursion_, _automatic recursion_, and
_recursive bindings_. Throughout this section of the article we'll spend some
time with each of these types of recursion, building up some examples and
working our way towards a better understanding of how they let us think about
the general nature of recursion and how it relates to fixed points.

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
_shape_ as our overall function. Whenever we're writing a recursive function, it
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
factorial 3 =     (3 * 2 * 1)

factorial 4 = 4 * (factorial 3)
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
structures, we center the relevant bits of our code. Finally, and perhaps most
importantly, functions like folds give us a common language for talking about
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

The `fix` function, defined in `Data.Function` in `base`gives us another way to
approach recursion in haskell. Let's start by taking a look at
[the documentation for fix](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Function.html#v:fix):

### Fix By The Docs
For ease of readability, the documentation for `fix` is reproduced below:

[fix](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Function.html#v:fix)
 `f`is the least fixed point of the function `f`, i.e. the least defined `x`
such that `f x = x`.

For example, we can write the factorial function using direct recursion as

```haskell
λ let fac n = if n <= 1 then 1 else n * fac (n-1) in fac 5
120
```

This uses the fact that Haskell’s `let` introduces recursive bindings. We can
rewrite this definition using 'fix',

```haskell
λ fix (\rec n -> if n <= 1 then 1 else n * rec (n-1)) 5
120
```
Instead of making a recursive call, we introduce a dummy parameter `rec`; when
used within `fix`, this parameter then refers to `fix`’s argument, hence the
recursion is reintroduced.  `fix :: (a -> a) -> a`

### Untangling the Type of fix

Whenever we want to understand something new in haskell, a good first instinct
is to start by looking at the types, as this tells us quite a bit about what a
function can, and often more importantly _can't_ do.

The type of `fix :: (a -> a) -> a` tells us that it's going to take a function,
and return a value. For the sake of discussion, let's give the function that we
pass into `fix` the name`g`. So, `g :: a -> a` and `fix g :: a`.

At first look, this might not look all that difficult at all. `fix` just needs
to call `g` with a value to get a value back out that it can return. We can
imagine any number of similar functions that would work for some specific type,
an `Int`:

```haskell
applyZero :: (Int -> Int) -> Int
applyZero g = g 0
```

Similarly, we can think of any number of candidates for `g` that we could pass
in and get a good result back out:

```haskell
λ applyZero (+1)
1
λ applyZero (*4)
0
```

Unfortunately, this relies on the fact that `applyZero` can pick some number to
pass in. It can do that because we know that it's working with `Int` values, so
we can pick an `Int` value to pass into it. `fix` doesn't have things so easy-
since `a` could be _anything_ there's no value we can pick to pass into `g` to
get back a value.

We can see this play out if we try to pass some function, like `(+1)` into
`fix`:  It will never give us back a value, because _it can't_. You can try it
yourself in ghci. When you are satisfied that you won't get back a value, you
can type `control`+`c` to cancel the current function.

```haskell
λ fix (+1)
Interrupted.
```

The trick to `fix` is that, sometimes, it can give back a result. It can do that
when the final value that you get back out doesn't depend on any particular
input value. For example, if we use the `const` function, which ignores any
argument passed into it and just returns a value, then we can get a result from
`fix`:

```haskell
λ fix (const "hello, haskell")
"hello, haskell"
```

Ignoring the question of _how this could possibly work_, it makes sense. The
definition of a fixed point of a function is that it's a value that, when passed
into a function, causes the function to return that same value. This is exactly
what `const` does- ignores its input and returns some value:

```haskell
λ :t const
const :: a -> b -> a
λ :t const "foo"
const "foo" :: b -> [Char]
λ f = const "foo"
λ f 1
"foo"
```

This means that whatever value we pass into `const` will be the fixed point of
the function that it returns.

Outside of the mathematical definition of a fixed point, the behavior of `fix`
also makes sense if we think about it in terms of laziness, and
computability. We've already noted that because the `fix` is polymorphic, fixed
itself can't ever get a value to pass into the function it's trying to find the
fixed point of. In a strictly evaluated language, that would be a problem, but
thanks to haskell's laziness, _“a value we can't ever actually compute”_ is
still something that we can work with.

In the case of `fix`, the parameter that it passes into its function _might_ be
a value that we can't ever actually compute, but it turns that that's actually
perfectly okay so long as we never try to compute it. In other words, if the
function we pass in is _lazy_ in its argument, then we never try to run the
impossible calculation of creating a value, and so everything works out.

### The Two-Argument Conundrum

Now that you understand how `fix` can take advantage of laziness to work at all,
there's another aspect to `fix` that might trip you up reading through the
documentation.  Recall that the type of fix is `fix :: (a -> a) -> a`, but what
the documentation passes in a factorial function that takes two arguments:

```haskell
λ fix (\rec n -> if n <= 1 then 1 else n * rec (n-1)) 5
120
```

We can factor the function out from this example and give it a name, and confirm
that it does, in fact, take two arguments: `rec`, a function with type `(p ->
p)`, and `n`, a value of type `p`.

```haskell
λ factorial = \rec n -> if n <= 1 then 1 else n * rec (n-1)
λ :t factorial
factorial :: (Ord p, Num p) => (p -> p) -> p -> p
```

So that it's a bit easier for us to talk about, let's pick some
specific type to use as we're thinking about this. For no particular reason,
let's use `Int`, so we can let `factorial` have the type:

```haskell
factorial :: (Int -> Int) -> Int -> Int
```

There are two things that we need to remember to be able to put together how
this works. The first is that it can sometimes be quite helpful for us to stop
and remember that haskell functions are curried, and to think through what our
type signatures really mean when we look at them.

We might naturally read the type `(Int -> Int) -> Int -> Int` as _a function
that takes a function from an Int to an Int, and an Int, returning an Int_. Most
of the time we can get by just fine when we read our function types this way,
but every once in a while it can throw us for a loop.

Since haskell functions are curried, a we can rewrite a function like:

```haskell
factorial :: (Int -> Int) -> Int -> Int
factorial = \rec n -> if n <= 1 then 1 else n * rec (n-1)
```

Into one that takes a single argument and returns a new function:

```haskell
factorial :: (Int -> Int) -> Int -> Int
factorial = \rec -> \n -> if n <= 1 then 1 else n * rec (n-1)
```

When we rewrite it that way, we might naturally want to describe the function
as: _a function that takes a function from and Int to an Int, and returns a
function from an Int to an Int_. We can rewrite our type signature to reflect
this restatement of our function so that it reads:

```haskell
factorial :: (Int -> Int) -> (Int -> Int)
factorial = \rec -> \n -> if n <= 1 then 1 else n * rec (n-1)
```

Looking at this rewritten type signature now, we can start to see the second
important thing that we need to keep in mind. When we're dealing with
polymorphic functions that take an `a`, the `a` could be _anything_, including a
function. If we replace the `a` type parameters with `(Int -> Int)` then the
type of fix would become:

```haskell
fixFactorial :: ((Int -> Int) -> (Int -> Int)) -> (Int -> Int)
```

Or, if we let ghci render the type for us without any unnecessary parentheses:

```haskell
fix @(Int -> Int) :: ((Int -> Int) -> Int -> Int) -> Int -> Int
```

In the next section we'll take a look at how `fix` is actually implemented. Once
you've had a chance to see the implementation, we'll come back to both the type
of fix and how it works with laziness and put all of that knowledge together
into a more cohesive understanding of how it actually works.

## Implementing fix

For all of the discussion about how `fix` works, its implementation is
remarkably short. Whenever we find ourselves facing something completely unknown
in haskell, we can start by looking at the types, and the next step is often to
read the source code.  [The source code for fix](https://hackage.haskell.org/package/base-4.15.0.0/docs/src/Data-Function.html#fix
is available on hackage, and it's quite short:

```haskell
fix :: (a -> a) -> a
fix f = let x = f x in x
```

Let's walk through what's happening here and see if we can get a handle on
it. We start with a parameter, `f`, which is whatever function we want to find
the fixed point for.

Next, we create a _recursive let binding_ where we define `x` to be the result
of applying `f` to `x`. This recursive let binding is the magic behind how the
fixed point calculation works.

When we first call `fix` and create the let binding where we define `x`, we know
that it has to have the _type_ a, and a value that, when it's needed, will be
computed by the expression `f x`.

The `x` in that computation, likewise, isn't a value yet. It's a _thunk_ that,
_if_ it is evaluated, will be computed by calling `f x`. In other words, we
start with:

```haskell
fix f = let x = {- <some unevaluated thunk> -} in x
```

If whoever calls this function decides they need the value of `x`, then they'll
get:

```haskell
fix f = let x = f {- <some unevaluated thunk> -} in x
```

If `f` is a function like `const` that always returns a value without ever
looking at its input value, then `x` will get set to that value and can be
evaluated without any issues at all.

On the other hand, if `f` does need to evaluate `x`, like when we tried to pass
in `(+1)`, we'll end up with a computation that can never complete, because each
time we try to look at `x` we'll get back another layer of _some unevaluated
thunk_. On the surface, this might seem to be a bit limited. After all, if we
need to pass in a function that always returns a value and never looks at its
input, we're limited to permutations of `const` and not much else, unless we can
get some data to work with from somewhere else...

## Tying The Knot

The `fix` function doesn't require a function that _never_ evaluates its
argument in order to eventually give us back a value. Instead, we need to give
it a function that _eventually_ doesn't evaluate its argument. The one-word
difference here between _never_ and _eventually_ is the difference between a
computation that terminates and is well-defined, and one that is
`undefined`. This is where passing a function of two parameters into fix comes
into play. When we have a function like `(Int -> Int)` there's no option except
for the input value that we're given to decide _when_ to terminate, so we always
have to evaluate it. On the other hand, a function with the type `(Int -> Int)
-> (Int -> Int)` has much more flexibility.  To see how, let's go back to our
definition of `factorial`:

```haskell
factorial :: (Int -> Int) -> (Int -> Int)
factorial rec = \n -> if n <= 1 then 1 else n * rec (n-1)
```

In this factorial function, we're taking a parameter, `rec :: Int -> Int`, but
we only ever evaluate it if `n` is greater than 1. Since `n` decreases with each
step, we know that it will eventually reach 1 (assuming we started with a
positive number), and so we know that `rec` will eventually not be evaluated,
and we can return a good value.

When we look at this deeply we can see that this is actually a really
interesting approach- we're taking advantage of laziness so that we can return a
function that only causes a value in its closure to be evaluated when the input
to the returned function is sufficiently high. It's almost like we're passing
information backwards through time, but in fact we're simply making use of the
behavior of lazy evaluation and the call stack to propagate information back and
eventually resolve some thunks that have been hanging out patiently waiting
around for us to allow them to be computed.

As a final exercise, let's walk through the example step by step to get a much
better idea of what's happening when we make use of `fix`.

## Fixing The Factorial

We'll start our manual evaluation by defining two functions:

```haskell
factorial' :: (Int -> Int) -> (Int -> Int)
factorial' rec = \n -> if n <= 1 then 1 else n * rec (n-1)

factorial :: Int -> Int
factorial = fix factorial'
```

In ghci we'll start by calling `factorial` with `5`:

```haskell
λ factorial 5
```

We can expand this to:

```haskell
fix (\rec n -> if n <= 1 then 1 else n * rec (n - 1)) 5
```

And that, in turn, becomes:

```haskell
let x = (\rec n -> if n <= 1 then 1 else n * rec (n - 1)) x in x 5
```

If we apply this function to 5, and replace `n` with `5` we end up with:

```haskell
let x = (\rec 5 ->
  if 5 <= 1 then 1 else 5 * rec (5 - 1)
  ) x
in x 5
```

Following the pattern until we get to our base case, we have:

```haskell
  let x = (\rec 5 ->
             if 5 <= 1 then 1 else 5 * rec (5 - 1)
          ) $ (\rec' 4 ->
             if 4 <= 1 then 1 else 4 * rec' (4 - 1))
          ) $ (\rec'' 3 ->
             if 3 <= 1 then 1 else 3 * rec'' (3 - 1))
          ) $ (\rec''' 2 ->
             if 2 <= 1 then 1 else 2 * rec''' (2 - 1))
          ) $ (\_rec 1 ->
             if 1 <= 1 then 1 else {- never evaluated #-}
          )
  in x $ 5
```

Once we finally hit the case where `n == 1` and we stop evaluating `rec` we can
start to resolve the stack of calls in reverse order, so `rec'''` becomes `1`
and we get:

```haskell
  let x = (\rec 5 ->
             if 5 <= 1 then 1 else 5 * rec (5 - 1)
          ) $ (\rec' 4 ->
             if 4 <= 1 then 1 else 4 * rec' (4 - 1))
          ) $ (\rec'' 3 ->
             if 3 <= 1 then 1 else 3 * rec'' (3 - 1))
          ) $ (\rec''' 2 ->
             if 2 <= 1 then 1 else 2 * 1)
          )
  in x $ 5
```

Which becomes:

```haskell
  let x = (\rec 5 ->
             if 5 <= 1 then 1 else 5 * rec (5 - 1)
          ) $ (\rec' 4 ->
             if 4 <= 1 then 1 else 4 * rec' (4 - 1))
          ) $ (\rec'' 3 ->
             if 3 <= 1 then 1 else 3 * 2
          )
  in x $ 5
```

And so on until we finally get to:

```haskell
  let x = (\_ 5 ->
             if 5 <= 1 then 1 else 5 * 4 * 3 * 2
          )
  in x $ 5
```

## Conclusion

In this post you've learned how the `fix` function from `Data.Function` relies
on important features of haskell, like laziness and recursive let bindings, to
provide us with a way of doing automatic recursion without having to ever
directly make a recursive call. By understanding how haskell's type system,
currying, and lazy evaluation work together, and taking time to sympathize with
the compiler and better understand how expressions are evaluated, you can start
to see precisely how some of the more interesting, and at first more
intimidating, areas of haskell work.
