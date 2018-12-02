Chapter 1: Computer Music, Euterpea and Haskell

> module Chapter1 where
> import Euterpea

This chapter introduces some basic Haskell and Euterpea concepts.

In section 1.7, our first interesting concepts are introduced.

1.7.1 & 1.7.2: Naming and functional abstraction
================================================

For example, the functions `note` and `rest`:

note :: Dur -> Pitch -> Music Pitch

That is, to produce a note, we need a duration and a pitch. For example:

> concertA = (A, 4)
> qnA = note (1/4) concertA


Is a quarter note concert A. Then there's `rest`

rest :: Dur -> Music Pitch.

Both notes and rests are the smallest units of playable music.

There's also the :+: and :=: operators:

(:+:) :: Music Pitch -> Music Pitch -> Music Pitch
(:=:) :: Music Pitch -> Music Pitch -> Music Pitch

Where

* `m1 :+: m2` is the music value produced by playing m1 followed by m2
* `m1 :=: m2` is the music value produced by playing m1 and m2 simultaneously.

Then there's also `trans`, which transposes a given pitch by a given number of semitones:

trans :: Int -> Pitch -> Pitch

consider the simple melody (where qn is a quarter note, note we defined Pitches):

> p1 = (D, 4)
> p2 = (F, 4)
> p3 = (A, 4)
> simple_mel = note qn p1 :+: note qn p2 :+: note qn p3

You can play the above melody by typing `play simple_mel` in the interactive haskell REPL.


If we want to harmonize with a note played a minor third lower (3 semitones):

> mel = (note qn p1 :=: note qn (trans (-3) p1)) :+:
>       (note qn p2 :=: note qn (trans (-3) p2)) :+:
>       (note qn p3 :=: note qn (trans (-3) p3))


You should be able to play the above melody by typing `play mel` in the repl.

However, given the repetition above when harmonizing, we'll introduce an abstraction:

> hNote :: Dur -> Pitch -> Music Pitch
> hNote d p = note d p :=: note d (trans (-3) p)

And we can now express the melody more succinctly:

> aMel :: Music Pitch
> aMel = hNote qn p1 :+: hNote qn p2 :+: hNote qn p3

Instead of introducing a top-level binding for hNote, we could introduce a local
one using `let`:

> lMel :: Music Pitch
> lMel = let hNote d p = note d p :=: note d (trans (-3) p)
>        in  hNote qn p1 :+: hNote qn p2 :+: hNote qn p3

Notice how we could define a local `hNote` and Haskell allowed us; this is due
to them being in different scopes.

1.7.3 Data abstraction
======================

Introduces the cons operator (`:`), which is right associative, so all of the below
are equivalent:

[C, D, Ef]
C : [D, Ef]
C : (D : (Ef : []))
C : D : Ef : []

(Which is reminiscent of exactly how cons cells in Lisp work).

It also highlights how operators in Haskell have both _precedence_ and
_associativity_.

Now we're going to define a function that takes list of pitches and returns a sequential composition of harmonized notes:

> hList :: Dur -> [Pitch] -> Music Pitch

We must consider all possible inputs. If empty, it should be a zero-duration rest:

> hList d [] = rest 0

If not empty, an element p followed by ps (other elements, plural of p); it should
return the harmonization of p followed by the sequential composition of the harmonization of ps.

> hList d (p : ps) = hNote d p :+: hList d ps

Note how the original problem was solved by breaking it down into subproblems, with the aid of recursion!

Given the above function, we can now define our melody more succinctly:

> bMel :: Music Pitch
> bMel = hList qn [p1, p2, p3]


Exercise: modify hNote and hList to parameterize the harmonization
------------------------------------------------------------------

> ghNote :: Dur -> Pitch -> Int -> Music Pitch
> ghNote d p i = note d p :=: note d (trans i p)
> ghList :: Dur -> [Pitch] -> Int -> Music Pitch
> ghList d [] i = rest 0
> ghList d (p:ps) i = ghNote d p i :+: ghList d ps i
> cMel = ghList qn [p1, p2, p3] (-5)

Note: wonder how I can make "i" a default/optional argument?

1.8: Haskell equality vs. musical equality
===========================================

Note that in the definition of hList, the terminus of the recursion is:

hNote qn p1 :+: hNote qn p2 :+: hNote qn p3 :+: rest 0

which _isn't_ the same as our original melody (adds the `rest 0`). However, musically, a zero duration rest doesn't change what we hear, so they're equal.
Euterpea defines this as the axiom:

m :+: rest 0 `is musically equivalent to` m

To illustrate musical equality, a more contrived example:

Given figure 1.1 (which shows three diatonic groups of quarter notes) harmonized
like `mel`:

> p4 = (Ef, 4)
> p5 = (F, 4)
> p6 = (G, 4)
> eMel = ghList qn [p4, p5, p6] (-3)

Which is equivalent to this polyphonic interpretation, where each pair
of notes is seen as a harmonic unit:

> eMel1 = (note qn p4) :=: (note qn (C,4)) :+:
>         (note qn p5) :=: (note qn (D,4)) :+:
>         (note qn p6) :=: (note qn (E,4))

It is also equivalent to a *contrapuntal* interpretation, which sees
two distinct voices, or melodic lines:

> eMel2 = (note qn p4 :+: note qn p5 :+: note qn p6)
>         :=:
>         (note qn (C,4) :+: note qn (D,4) :+: note qn (E,4))


These are clearly not the same _Haskell_ values, but they're musically equivalent.

The chapter ends with a small tangent into numerical representation, calling
out that Int comprises integers guaranteed to fit into a single word of memory,
whereas Integer comprises all integers and thus has dynamic memory allocation
(and is consequently less efficient).
