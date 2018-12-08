Chapter 3: Polymorphic and Higher Order Functions

> module Chapter3 where
> import Euterpea

This chapter introduces a ton of higher order functions and how they can aid
in computer music.

3.1: Map
========

Introduces `map` as a function with the signature:

map :: (a -> b) -> [a] -> [b]

The type defined above, with *type variables*, is said to be the *principal type*,
which is the least general type that captures all valid uses of the expression.

For example, let's generate a six-note whole-tone scale starting at a given pitch:

> a440 :: Pitch
> a440 = (A, 4)
> wts :: Pitch -> [Music Pitch]
> wts p = let f ap = note qn (pitch (absPitch p + ap))
>         in map f [0,2,4,6,8]

Which returns:

λ> wts a440
[Prim (Note (1 % 4) (A,4)),Prim (Note (1 % 4) (B,4)),Prim (Note (1 % 4) (Cs,5)),Prim (Note (1 % 4) (Ds,5)),Prim (Note (1 % 4) (F,5))]

Exercise 3.1:
-------------

Using `map` define:

1. A function f1 :: Int -> [Pitch] -> [Pitch] that transposes each pitch in its
   second argument by the amount specified in its first argument.

> f1 :: Int -> [Pitch] -> [Pitch]
> f1 t ps = map (trans t) ps -- currying trans with t

Solved: λ> f1 2 [(C, 4), (D, 4), (E, 4)]
        => [(D,4),(E,4),(Fs,4)]


2. A function f2 :: [Dur] -> [Music a] that turns a list of durations into a list
   of rests, each having the corresponding duration

> f2 :: [Dur] -> [Music a]
> f2 ds = let f d = rest d
>         in map f ds

Solution:
λ> f2 [1,2,3,0]
[Prim (Rest (1 % 1)),Prim (Rest (2 % 1)),Prim (Rest (3 % 1)),Prim (Rest (0 % 1))]



3. A function f3 :: [Music Pitch] -> [Music Pitch] that takes a list of music values
   assumed to be single notes and for each note, halves its duration and places
   a rest of the same duration after it (like a staccato interpretation of the notes)

> f3 :: [Music Pitch] -> [Music Pitch]
> f3 ps = let f (Prim (Note d p)) = note (1/2 * d) p :+: rest (1/2 * d)
>         in  map f ps


Solution:
λ> f3 [c 4 qn, d 4 en, e 4 hn]
[Prim (Note (1 % 8) (C,4)) :+: Prim (Rest (1 % 8)),Prim (Note (1 % 16) (D,4)) :+: Prim (Rest (1 % 16)),Prim (Note (1 % 4) (E,4)) :+: Prim (Rest (1 % 4))]
λ> en
1 % 8
λ> sn
1 % 16
λ> qn
1 % 4
λ> 


3.3 Append
==========

The append infix operator is defined as:

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

You can define your own infix operators, but they can't begin with colon: that's
for infix constructors.

3.4 Fold
========

Suppose we want to define a few functions:

Line gives us a sequence of notes and produces a melody

line :: [Music a] -> Music a
line [] = rest 0
line (m:ms) = m :+: line ms

Chord:

chord :: [Music a] -> Music a
chord [] = rest 0
chord (m:ms) = m :=: chord ms

Or maxPitch, to find the max of a list of pitches:

maxPitch :: [Pitch] -> Pitch
maxPitch [] = pitch 0
maxPitch (p:ps) = p !!! maxPitch ps

where (!!!) is defined as:

p1 !!! p2 = if absPitch p1 > absPitch p2 then p1 else p2

All of these can benefit from another higher order function, `fold`:

fold :: (a -> b -> b) -> b -> [a] -> b
fold op init [] = init
fold op init (x:xs) = x `op` fold op init xs

(notice that a `op` b is equivalent to op a b). Given `fold`,
we can redefine the functions as:

line, chord :: [Music a] -> Music a
line ms = fold (:+:) (rest 0) ms
chord ms = fold (:=:) (rest 0) ms

maxPitch :: [Pitch] -> Pitch
maxPitch ps = fold (!!!) (pitch 0) ps

(Notice that we send operators as function arguments by enclosing them
in parens).

3.4.1 Haskell's Folds
=====================

Haskell has two versions of fold: `foldr` and `foldl`:

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op init = init
foldr op init (x:xs) = x `op` foldr op init xs

whereas foldl:

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl op init = init
foldl op init (x:xs) = fold op (init `op` x) xs

Notice how it's rather obvious from which side they start _folding_.

And, for functions that shouldn't take empty lists, there's `foldr1` and `foldl1`:

line1, chord1 :: [Music a] -> Music a
line1 ms = foldr1 (:+:) ms
chord1 ms = folr1 (:=:) ms

3.4 Reverse
===========

One first approximation for `reverse` can be:

reverse :: [a] -> [a]

reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

But it's very inefficient!

Another definition:

reverse xs let rev acc [] = acc
               rev acc (x:xs) = rev (x:acc) xs
           in rev [] xs

It looks a lot like foldl if we add an auxiliary function, `revOp`

revOp a b = b : a
reverse xs = let rev op acc [] = acc
                 rev op acc (x:xs) = rev op (acc `op` x) xs
             in rev revOp [] xs

So we can define it in terms of fold!

reverse xs = fold revOp [] xs

3.6 Currying
============

Functions are curried by default if not applied to all the arguments they take,
so we can use this liberally. E.g. to redefine line and chord:

line = foldr (:+:) (rest 0) -- can now be applied to a list
chord = foldr (:=:) (rest 0)

and even hList, recently defined as:

hList d ps = let f p = hNote d p
             in line (map f ps)

hList d ps = let f = hNote d -- no longer need to tell it about `p`, since
                             -- we were just passing it through anyway
             in line (map f ps)

Which makes it so simple we can just say:

hList d ps = line (map (hNote d) ps)

Knowing about currying, we can further refine `reverse`, if we know `flip`:

flip :: (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x

so we can say that:

revOp acc x = flip (:) acc x

or even:

revOp = flip (:)

so we might as well say:

reverse = foldl (flip (:)) []

3.7 Errors
==========

error :: String -> a

can be used to raise an error:

foldr1 f [] = error "Prelude.foldr1: empty list"

Exercises
=========

3.9 Define a polymorphic function fuse :: [Dur] -> [Dur -> Music a] -> [Music a]
    that combines a list of durations with a list of notes without duration.

> fuse :: [Dur] -> [Dur -> Music a] -> [Music a]
> fuse [ ] [ ] = []
> fuse [_] [ ] = error "fuse: lists must be of equal lengths"
> fuse [ ] [_] = error "fuse: lists must be of equal lengths"
> fuse (d:ds) (m:ms) = (m d) : fuse ds ms

Solution:
λ> fuse [qn, hn, sn] [c 4, d 4, e 4]
[Prim (Note (1 % 4) (C,4)),Prim (Note (1 % 2) (D,4)),Prim (Note (1 % 16) (E,4))]

3.11 Define a function chrom :: Pitch -> Pitch -> Music Pitch such that
     chrom p1 p2 is a chromatic scale of quarter notes whose first pitch
     is p1 and last pitch is p2; if p1 > p2, the scale should be descending.
     if p1 == p2, it should contain one note.

reverse xs let rev acc [] = acc
               rev acc (x:xs) = rev (x:acc) xs
           in rev [] xs

Recursive solution:

> apRange :: AbsPitch -> AbsPitch -> [AbsPitch]
> apRange ap1 ap2 = let apr start end acc
>                         | start == end = acc ++ [start]
>                         | start >  end = apr (start - 1) end (acc ++ [start])
>                         | start <  end = apr (start + 1) end (acc ++ [start])
>                   in apr ap1 ap2 []
> chrom :: Pitch -> Pitch -> Music Pitch
> chrom p1 p2 = let absPitches = apRange (absPitch p1) (absPitch p2)
>                   apToNote ap  = note qn (pitch ap)
>               in foldr (:+:) (rest 0) (map apToNote absPitches)


λ> chrom (A, 4) (D, 4)
Prim (Note (1 % 4) (A,4)) :+: (Prim (Note (1 % 4) (Gs,4)) :+: (Prim (Note (1 % 4) (G,4)) :+: (Prim (Note (1 % 4) (Fs,4)) :+: (Prim (Note (1 % 4) (F,4)) :+: (Prim (Note (1 % 4) (E,4)) :+: (Prim (Note (1 % 4) (Ds,4)) :+: (Prim (Note (1 % 4) (D,4)) :+: Prim (Rest (0 % 1)))))))))
λ> chrom (A, 4) (A, 4)
Prim (Note (1 % 4) (A,4)) :+: Prim (Rest (0 % 1))
λ> chrom (D, 4) (A, 4)
Prim (Note (1 % 4) (D,4)) :+: (Prim (Note (1 % 4) (Ds,4)) :+: (Prim (Note (1 % 4) (E,4)) :+: (Prim (Note (1 % 4) (F,4)) :+: (Prim (Note (1 % 4) (Fs,4)) :+: (Prim (Note (1 % 4) (G,4)) :+: (Prim (Note (1 % 4) (Gs,4)) :+: (Prim (Note (1 % 4) (A,4)) :+: Prim (Rest (0 % 1)))))))))
λ> 

3.12 Abstractly, a scale can be defined as a sequence of its intervals.
     Define a function, mkScale :: Pitch -> [Int] -> Music Pitch that
     constructs a scale for pitch `p` with [ints] intervals

Am I cheating for using scanl? http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:scanl

> mkScale :: Pitch -> [Int] -> Music Pitch
> mkScale p ints = let absPitches = scanl (+) (absPitch p) ints
>                      apToNote ap = note qn (pitch ap)
>                  in  foldr (:+:) (rest 0) (map apToNote absPitches)

λ> mkScale (C, 4) [2,2,1,2,2,2]
Prim (Note (1 % 4) (C,4)) :+: (Prim (Note (1 % 4) (D,4)) :+: (Prim (Note (1 % 4) (E,4)) :+: (Prim (Note (1 % 4) (F,4)) :+: (Prim (Note (1 % 4) (G,4)) :+: (Prim (Note (1 % 4) (A,4)) :+: (Prim (Note (1 % 4) (B,4)) :+: Prim (Rest (0 % 1))))))))
λ> play (mkScale (C, 4) [2,2,1,2,2,2])
λ> play (mkScale (C, 4) [2,2,1,2,2,2])
λ> play (mkScale (C, 4) [2,2,1,2,2,2,1])
λ> play (mkScale (D, 4) [2,2,1,2,2,2,1])
λ> mkScale (A, 4) [2,1,2,2,1,2,1]
Prim (Note (1 % 4) (A,4)) :+: (Prim (Note (1 % 4) (B,4)) :+: (Prim (Note (1 % 4) (C,5)) :+: (Prim (Note (1 % 4) (D,5)) :+: (Prim (Note (1 % 4) (E,5)) :+: (Prim (Note (1 % 4) (F,5)) :+: (Prim (Note (1 % 4) (G,5)) :+: (Prim (Note (1 % 4) (Gs,5)) :+: Prim (Rest (0 % 1)))))))))
λ> mkScale (A, 4) [2,1,2,2,1,2,2]
Prim (Note (1 % 4) (A,4)) :+: (Prim (Note (1 % 4) (B,4)) :+: (Prim (Note (1 % 4) (C,5)) :+: (Prim (Note (1 % 4) (D,5)) :+: (Prim (Note (1 % 4) (E,5)) :+: (Prim (Note (1 % 4) (F,5)) :+: (Prim (Note (1 % 4) (G,5)) :+: (Prim (Note (1 % 4) (A,5)) :+: Prim (Rest (0 % 1)))))))))
λ> play (mkScale (A, 4) [2,1,2,2,1,2,2])
λ> play (mkScale (D, 4) [2,1,2,2,1,2,2])
λ> play (mkScale (C, 4) [2,1,2,2,1,2,2])
λ> mkScale (C, 4) [2,1,2,2,1,2,2]
Prim (Note (1 % 4) (C,4)) :+: (Prim (Note (1 % 4) (D,4)) :+: (Prim (Note (1 % 4) (Ds,4)) :+: (Prim (Note (1 % 4) (F,4)) :+: (Prim (Note (1 % 4) (G,4)) :+: (Prim (Note (1 % 4) (Gs,4)) :+: (Prim (Note (1 % 4) (As,4)) :+: (Prim (Note (1 % 4) (C,5)) :+: Prim (Rest (0 % 1)))))))))
λ>

3.13 Given one of the major modes, generate a scale:
https://en.wikipedia.org/wiki/Mode_(music)#Modern_modes

(already defined in euterpea:)
dataMode = Ionian | Dorian | Phrygian
  | Lydian | Mixolydian | Aeolian
  | Locrian

> genScale :: Mode -> Pitch -> Music Pitch
> genScale m p = let ints = case m of
>                             Ionian -> [2,2,1,2,2,2,1]
>                             Dorian -> [2,1,2,2,2,1,2]
>                             Phrygian -> [1,2,2,2,1,2,2]
>                             Lydian -> [2,2,2,1,2,2,1]
>                             Mixolydian -> [2,2,1,2,2,1,2]
>                             Aeolian -> [2,1,2,2,1,2,2]
>                             Locrian -> [1,2,2,1,2,2,2]
>                in mkScale p ints

3.14 Write the melody of Frère Jacques. Then, using functions already defined,
     generate a four-part round (four _identical_ voices), each delayed succesively
     by two measures. Use a different instrument for each voice.

https://en.wikipedia.org/wiki/Fr%C3%A8re_Jacques#/media/File:YB4001Canon_Frere_Jacques.png

> frereJacques :: Music Pitch
> frereJacques = line [g 4 qn, a 4 qn, b 4 qn, g 4 qn,
>                      g 4 qn, a 4 qn, b 4 qn, g 4 qn,
>                      b 4 qn, c 5 qn, d 5 hn,
>                      b 4 qn, c 5 qn, d 5 hn,
>                      d 5 den, e 5 sn, d 5 en, c 5 en, b 4 qn, g 4 qn,
>                      d 5 den, e 5 sn, d 5 en, c 5 en, b 4 qn, g 4 qn,
>                      g 4 qn, e 4 qn, g 4 hn,
>                      g 4 qn, e 4 qn, g 4 hn]
> shortFrereJacques :: Music Pitch
> shortFrereJacques = line [g 4 qn, a 4 qn, b 4 qn, g 4 qn,
>                           b 4 qn, c 5 qn, d 5 hn,
>                           d 5 den, e 5 sn, d 5 en, c 5 en, b 4 qn, g 4 qn,
>                           g 4 qn, e 4 qn, g 4 hn]

General fns: notice that `reductions` is my version of `scanl` as used above:

> reductions :: (b -> a -> b) -> b -> [a] -> [b]
> reductions f init [] = init : [] -- turn a single value into a list
> reductions f init (x:xs) = init : (reductions f (f init x) xs)
> repeat' :: Num b => Ord b => a -> b -> [a]
> repeat' a times  = let r y 0 ys = ys
>                        r y n ys = r y (n-1) (y:ys)
>                    in r a times []
> each :: (a -> b) -> [a] -> [b]
> each f [] = []
> each f (x:xs) = (f x):(each f xs)
> range :: Ord a => Num a => a -> [a]
> range n = reductions (+) 0 (repeat' 1 n)

Music fns:

> padl :: Music a -> Dur -> Int -> Music a
> padl mel delayVal delayTimes = (line (repeat' (rest delayVal) delayTimes)) :+: mel
> voices :: Music a -> Int -> Music a
> voices mel nVoices = foldl1 (:=:) (each (padl mel hn) (range (nVoices - 1)))
> canon :: Music a -> Int -> [InstrumentName] -> Music a
> canon mel nVoices instruments =
>   let voice m dur n = instrument (instruments !! n) (padl m dur n)
>   in foldl1 (:=:) (each (voice mel hn) (range (nVoices - 1)))

This generates something you can feed to `play`:

λ> (canon shortFrereJacques 4 [AcousticGrandPiano, PizzicatoStrings, Piccolo, Glockenspiel])
((Modify (Instrument AcousticGrandPiano) (Prim (Rest (0 % 1)) :+: (Prim (Note (1 % 4) (G,4)) :+: (Prim (Note (1 % 4) (A,4)) :+: (Prim (Note (1 % 4) (B,4)) :+: (Prim (Note (1 % 4) (G,4)) :+: (Prim (Note (1 % 4) (B,4)) :+: (Prim (Note (1 % 4) (C,5)) :+: (Prim (Note (1 % 2) (D,5)) :+: (Prim (Note (3 % 16) (D,5)) :+: (Prim (Note (1 % 16) (E,5)) :+: (Prim (Note (1 % 8) (D,5)) :+: (Prim (Note (1 % 8) (C,5)) :+: (Prim (Note (1 % 4) (B,4)) :+: (Prim (Note (1 % 4) (G,4)) :+: (Prim (Note (1 % 4) (G,4)) :+: (Prim (Note (1 % 4) (E,4)) :+: (Prim (Note (1 % 2) (G,4)) :+: Prim (Rest (0 % 1))))))))))))))))))) :=: Modify (Instrument PizzicatoStrings) ((Prim (Rest (1 % 2)) :+: Prim (Rest (0 % 1))) :+: (Prim (Note (1 % 4) (G,4)) :+: (Prim (Note (1 % 4) (A,4)) :+: (Prim (Note (1 % 4) (B,4)) :+: (Prim (Note (1 % 4) (G,4)) :+: (Prim (Note (1 % 4) (B,4)) :+: (Prim (Note (1 % 4) (C,5)) :+: (Prim (Note (1 % 2) (D,5)) :+: (Prim (Note (3 % 16) (D,5)) :+: (Prim (Note (1 % 16) (E,5)) :+: (Prim (Note (1 % 8) (D,5)) :+: (Prim (Note (1 % 8) (C,5)) :+: (Prim (Note (1 % 4) (B,4)) :+: (Prim (Note (1 % 4) (G,4)) :+: (Prim (Note (1 % 4) (G,4)) :+: (Prim (Note (1 % 4) (E,4)) :+: (Prim (Note (1 % 2) (G,4)) :+: Prim (Rest (0 % 1)))))))))))))))))))) :=: Modify (Instrument Piccolo) ((Prim (Rest (1 % 2)) :+: (Prim (Rest (1 % 2)) :+: Prim (Rest (0 % 1)))) :+: (Prim (Note (1 % 4) (G,4)) :+: (Prim (Note (1 % 4) (A,4)) :+: (Prim (Note (1 % 4) (B,4)) :+: (Prim (Note (1 % 4) (G,4)) :+: (Prim (Note (1 % 4) (B,4)) :+: (Prim (Note (1 % 4) (C,5)) :+: (Prim (Note (1 % 2) (D,5)) :+: (Prim (Note (3 % 16) (D,5)) :+: (Prim (Note (1 % 16) (E,5)) :+: (Prim (Note (1 % 8) (D,5)) :+: (Prim (Note (1 % 8) (C,5)) :+: (Prim (Note (1 % 4) (B,4)) :+: (Prim (Note (1 % 4) (G,4)) :+: (Prim (Note (1 % 4) (G,4)) :+: (Prim (Note (1 % 4) (E,4)) :+: (Prim (Note (1 % 2) (G,4)) :+: Prim (Rest (0 % 1)))))))))))))))))))) :=: Modify (Instrument Glockenspiel) ((Prim (Rest (1 % 2)) :+: (Prim (Rest (1 % 2)) :+: (Prim (Rest (1 % 2)) :+: Prim (Rest (0 % 1))))) :+: (Prim (Note (1 % 4) (G,4)) :+: (Prim (Note (1 % 4) (A,4)) :+: (Prim (Note (1 % 4) (B,4)) :+: (Prim (Note (1 % 4) (G,4)) :+: (Prim (Note (1 % 4) (B,4)) :+: (Prim (Note (1 % 4) (C,5)) :+: (Prim (Note (1 % 2) (D,5)) :+: (Prim (Note (3 % 16) (D,5)) :+: (Prim (Note (1 % 16) (E,5)) :+: (Prim (Note (1 % 8) (D,5)) :+: (Prim (Note (1 % 8) (C,5)) :+: (Prim (Note (1 % 4) (B,4)) :+: (Prim (Note (1 % 4) (G,4)) :+: (Prim (Note (1 % 4) (G,4)) :+: (Prim (Note (1 % 4) (E,4)) :+: (Prim (Note (1 % 2) (G,4)) :+: Prim (Rest (0 % 1)))))))))))))))))))
