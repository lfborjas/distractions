Chapter 5: Syntactic Magic

> module Chapter5 where
> import Euterpea

Introduces some syntactic sugar: anonymous functions, list comprehensions,
function composition and the `$` operator and "higher order thinking".

Most of this I applied in my `a musical offering` talk:
https://github.com/lfborjas/MusicalOffering

so I'm not taking a lot of official notes here.


5.1 Sections
============

Partial application of infix operators, e.g.

> playable :: [AbsPitch] -> [AbsPitch]
> playable = filter (<100)

λ> playable [101, 98, 2]
[98,2]

5.2 Anonymous functions
=======================

> dAugment :: [Music a] -> [Music a]
> dAugment = map (\(Prim (Note d p)) -> (Prim (Note (2*d) p)))


λ> dAugment [d 4 qn, fs 4 qn]
[Prim (Note (1 % 2) (D,4)),Prim (Note (1 % 2) (Fs,4))]

Note that, given the notation of lambdas, they can only match against one
argument (vs. regular fn definition which can match against many). This leaves
open the possibility of non-exhaustive pattern-matching errors at runtime.

5.3 List comprehensions
========================

> addDur :: Dur -> [Dur -> Music a] -> Music a
> addDur d ns = line [n d | n <- ns]

5.3.1 Arithmetic sequences
--------------------------

[n..] -- infinite list with a `step` of 1
  take 3 [1..] = 1,2,3
[n,n'..] -- infinite list where the `step` is n'-n
  take 3 [1,3..] = 1,3,5
[n..m] -- finite list with step = 1
[n,n'..m] -- finite list with step = n'-n


Exercise 5.4:

apPairs, which takes two lists of absolute pitches and zips them together (but only when the gap is > 2 and < 8)
And another function that plays pairs in parallel and strings those in a sequence --
make it alternate en and sn durations based on the first note's absPitch's parity.
Test by feeding it arithmetic sequences

> apPairs :: [AbsPitch] -> [AbsPitch] -> [(AbsPitch, AbsPitch)]
> apPairs aps1 aps2 =
>   [ (ap1, ap2) |
>     ap1 <- aps1, ap2 <- aps2 ,
>     let gap = (abs $ ap1 - ap2),
>     gap > 2, gap < 8 ]

testing it:

λ>  apPairs [1..2] [3..8]
[(1,4),(1,5),(1,6),(1,7),(1,8),(2,5),(2,6),(2,7),(2,8)]

And now the function that generates the so-called melody

> pairsToMel :: [(AbsPitch, AbsPitch)] -> Music Pitch
> pairsToMel pairs =
>   line $
>   [ n1 :=: n2 | (ap1, ap2) <- pairs, -- notice the destructuring
>     let n1 = toN ap1                 -- notice the use before defining
>         n2 = toN ap2                 -- and notice the lack of commas within the let
>         toN n = note dur $ pitch n
>         dur = (if (even ap2) then en else sn)]

Testing it:

λ> pairsToMel $ apPairs [60..62] [64..68]
(Prim (Note (1 % 8) (C,4)) :=: Prim (Note (1 % 8) (E,4))) :+: ((Prim (Note (1 % 16) (C,4)) :=: Prim (Note (1 % 16) (F,4))) :+: ((Prim (Note (1 % 8) (C,4)) :=: Prim (Note (1 % 8) (Fs,4))) :+: ((Prim (Note (1 % 16) (C,4)) :=: Prim (Note (1 % 16) (G,4))) :+: ((Prim (Note (1 % 8) (Cs,4)) :=: Prim (Note (1 % 8) (E,4))) :+: ((Prim (Note (1 % 16) (Cs,4)) :=: Prim (Note (1 % 16) (F,4))) :+: ((Prim (Note (1 % 8) (Cs,4)) :=: Prim (Note (1 % 8) (Fs,4))) :+: ((Prim (Note (1 % 16) (Cs,4)) :=: Prim (Note (1 % 16) (G,4))) :+: ((Prim (Note (1 % 8) (Cs,4)) :=: Prim (Note (1 % 8) (Gs,4))) :+: ((Prim (Note (1 % 16) (D,4)) :=: Prim (Note (1 % 16) (F,4))) :+: ((Prim (Note (1 % 8) (D,4)) :=: Prim (Note (1 % 8) (Fs,4))) :+: ((Prim (Note (1 % 16) (D,4)) :=: Prim (Note (1 % 16) (G,4))) :+: ((Prim (Note (1 % 8) (D,4)) :=: Prim (Note (1 % 8) (Gs,4))) :+: Prim (Rest (0 % 1))))))))))))))
λ> 

5.4 Function Composition
========================

Further simplification of our good old hList fn:

hList d ps = line (map (hNote d) ps)

Can now be:

hList d = line . map (hNote d)

5.5 Higher-order thinking
==========================

Consider the signatures for some common fns:

map :: (a -> b) -> [a] -> [b]
filter :: (a -> Bool) -> ([a] -> [a])
(.) :: (b->c) -> (a->b) -> a -> c

They can be rewritten to emphasize that what they return, really, is a function:

map :: (a -> b) -> ([a] -> [b]) -- a fn that transforms as into bs
filter :: (a->Bool) -> ([a] -> [a]) -- a fn that returns some as
(.) :: (b->c) -> (a->b) -> (a->c) -- a fn that now turns as into cs

Exercise 5.5

Rewrite hList with _total_ currying simplification (i.e. there's a `d` in there still). It's possible, but a bit perverse and not good haskell style!

> hNote :: Dur -> Pitch -> Music Pitch
> hNote d p = note d p :=: note d (trans (-3) p)
> hList' :: Dur -> [Pitch] -> Music Pitch
> hList' d = line . map (hNote d)
> hList :: Dur -> [Pitch] -> Music Pitch
> hList = map hNote

The farthest I got was

hList = map hNote

To get this compilation error:

chapter5.lhs:128:11-19: error: …
    • Couldn't match type ‘[Pitch -> Music Pitch]’
                     with ‘[Pitch] -> Music Pitch’
      Expected type: Dur -> [Pitch] -> Music Pitch
        Actual type: [Dur] -> [Pitch -> Music Pitch]
    • Possible cause: ‘map’ is applied to too many arguments
      In the expression: map hNote
      In an equation for ‘hList’: hList = map hNote
    |
Compilation failed.
