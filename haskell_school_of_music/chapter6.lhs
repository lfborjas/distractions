Chapter 6: More Music

> module Chapter6 where
> import Euterpea

Explores strategies for manipulating musical structures.

To play around with things, I'm introducing Bach's motif from
die Kunst der Fuge:

> motif :: Music Pitch
> motif = line [ d 4 hn, a 4 hn, f 4 hn, d 4 hn,
>                cs 4 hn, d 4 qn, e 4 qn, f 4 (hn + en),
>                g 4 en, f 4 en, e 4 en, d 4 qn]

6.1 Delay and Repeat
===================

Euterpea has the very useful `offset` and `times` functions

offset :: Dur -> Music a -> Music a
offset d m = rest d :+: m

times 0 m = rest 0
times n m = m :+: times (n-1) m

As well as "forever":

forever :: Music a -> Music a
forever m = m :+: forever m

6.2 Inversion and Retrograde
============================

The notions of inversion, retrograde, retrogade inversion, etc. can be captured in Euterpea.

Since all of them operate on a `line`, let's define a helper

lineToList :: Music a -> [Music a]
lineToList (Prim (Rest 0)) = []
lineToList (n :+: ns) = n : lineToList ns
lineToList _ = error "only for lines"

invert can now be defined (already ships with Euterpea, so doing a prime):

> invert' :: Music Pitch -> Music Pitch
> invert' m =
>   let l@(Prim (Note _ r):_) = lineToList m
>       inv (Prim (Note d p)) =
>         note d (pitch (2*absPitch r - absPitch p))
>       inv (Prim (Rest d)) = rest d
>   in line (map inv l)

λ> invert motif
Prim (Note (1 % 2) (D,4)) :+: (Prim (Note (1 % 2) (G,3)) :+: (Prim (Note (1 % 2) (B,3)) :+: (Prim (Note (1 % 2) (D,4)) :+: (Prim (Note (1 % 2) (Ds,4)) :+: (Prim (Note (1 % 4) (D,4)) :+: (Prim (Note (1 % 4) (C,4)) :+: (Prim (Note (5 % 8) (B,3)) :+: (Prim (Note (1 % 8) (A,3)) :+: (Prim (Note (1 % 8) (B,3)) :+: (Prim (Note (1 % 8) (C,4)) :+: (Prim (Note (1 % 4) (D,4)) :+: Prim (Rest (0 % 1)))))))))))))
λ> motif
Prim (Note (1 % 2) (D,4)) :+: (Prim (Note (1 % 2) (A,4)) :+: (Prim (Note (1 % 2) (F,4)) :+: (Prim (Note (1 % 2) (D,4)) :+: (Prim (Note (1 % 2) (Cs,4)) :+: (Prim (Note (1 % 4) (D,4)) :+: (Prim (Note (1 % 4) (E,4)) :+: (Prim (Note (5 % 8) (F,4)) :+: (Prim (Note (1 % 8) (G,4)) :+: (Prim (Note (1 % 8) (F,4)) :+: (Prim (Note (1 % 8) (E,4)) :+: (Prim (Note (1 % 4) (D,4)) :+: Prim (Rest (0 % 1)))))))))))))
λ>

What is inversion?

* It is done with respect to a given pitch (by convention, the first of the line)
* Given a first pitch `r` and a line `l`, the intervals of the other pitches in
  the line are negated (inverted). Thus, given an absolute pitch of ap for r:
  the inverted pitch of any `p` is ap - (absPitch(p) - ap), or 2*ap - absPitch p

For example, inverting the motif above gives us:

pitch    inverse
A        G       (the distance from D to A is the same as G to D, i.e. we invert
                  going up from D to going down from D--or up _to_ D)
F        B
Ds       Cs


We can now define other interesting 12-tone functions (also ship with Euterpea):

> retro', retroInvert', invertRetro' :: Music Pitch -> Music Pitch
> retro' = line . reverse . lineToList
> retroInvert' = retro' . invert'
> invertRetro'  = invert' . retro' 

6.3 Computing Duration
======================

We sometimes need to know the total duration of a given Music value, thus:

(also ships with Euterpea):

> dur' :: Music a -> Dur
> dur' (Prim (Note d _))    = d
> dur' (Prim (Rest d))      = d
> dur' (m1 :+: m2)          = dur' m1 + dur' m2
> dur' (m1 :=: m2)          = dur m1 `max` dur m2 --whichever is longest
> dur' (Modify (Tempo r) m) = dur m/r -- need to scale by the modifying tempo
> dur' (Modify _  m)        = dur m -- other modifiers don't have a bearing here.


6.4 Super Retrograde
====================

There's a flaw in our first approach to retrograde (here, `retro'`): it's defined
in terms of `lineToList`, which assumes a simple line (a :+: b :+: c) where each of , b and c are notes.
It doesn't recursively go down into each component and reverts those, nor does it
work for musical values that have chords.
I had this exact issue in the crabCanon exercise: couldn't prove that playing
it backwards was the same as playing it forwards, since my `retrograde` was
this naïve version:

https://github.com/lfborjas/MusicalOffering/blob/master/MusicalOffering/CrabCanon/Performance.lhs#L183-L184

As a practical example:

A simple melody can't be reverted unless it _looks_ like something `line` would build
(i.e. with a `rest 0` to indicate the end):

λ> retro' $ (c 4 qn :+: e 4 qn) :+: (g 4 qn :+: bf 4 qn)
*** Exception: lineToList: argument not created by function line
CallStack (from HasCallStack):
  error, called at ./Euterpea/Music.lhs:295:7 in Euterpea-2.0.6-sUxZzvXR5hKuIjgYiTYLc:Euterpea.Music

End even if that's fixed, note that the innermost lines are _not_  reverted:

λ> retro' $ (c 4 qn :+: e 4 qn) :+: (g 4 qn :+: bf 4 qn) :+: rest 0
(Prim (Note (1 % 4) (G,4)) :+: Prim (Note (1 % 4) (Bf,4))) :+: ((Prim (Note (1 % 4) (C,4)) :+: Prim (Note (1 % 4) (E,4))) :+: Prim (Rest (0 % 1)))

And chords have no salvation:

λ> retro' $ (c 4 qn :=: e 4 qn) :=: (g 4 qn :=: bf 4 qn) :+: rest 0
*** Exception: lineToList: argument not created by function line
CallStack (from HasCallStack):
  error, called at ./Euterpea/Music.lhs:295:7 in Euterpea-2.0.6-sUxZzvXR5hKuIjgYiTYLc:Euterpea.Music


However, now that we have `dur`, we can invert _any_ Music Value, not just simple lines:

> retro'' :: Music a -> Music a
> retro'' n@(Prim _) = n
> retro'' (Modify c m) = Modify c (retro'' m)
> retro'' (m1 :+: m2)  = retro'' m2 :+: retro'' m1
> retro'' (m1 :=: m2)  =
>   let d1 = dur m1
>       d2 = dur m2
>   in if d1 > d2 then retro'' m1 :=: (rest (d1 - d2) :+: retro'' m2)
>      else (rest (d2 - d1) :+: retro'' m1) :=: retro'' m2 -- adds a silence so the shorter line starts after longer so they end at the same time (symmetrical to the longer ending after the shorter when they begin at the same time)

Which does the right thing:

λ> retro'' $ (c 4 qn :=: e 4 qn) :=: (g 4 qn :=: bf 4 qn) :+: rest 0
(Prim (Rest (0 % 1)) :+: ((Prim (Rest (0 % 1)) :+: Prim (Note (1 % 4) (C,4))) :=: Prim (Note (1 % 4) (E,4)))) :=: (Prim (Rest (0 % 1)) :+: ((Prim (Rest (0 % 1)) :+: Prim (Note (1 % 4) (G,4))) :=: Prim (Note (1 % 4) (Bf,4))))
λ> retro'' $ (c 4 qn :+: e 4 qn) :+: (g 4 qn :+: bf 4 qn) :+: rest 0
(Prim (Rest (0 % 1)) :+: (Prim (Note (1 % 4) (Bf,4)) :+: Prim (Note (1 % 4) (G,4)))) :+: (Prim (Note (1 % 4) (E,4)) :+: Prim (Note (1 % 4) (C,4)))

And _this_ is the version that ships as `retro` in Euterpea.

6.5 Cut and Remove
==================

`cut` takes a given duration of notes from a Music value, discards the rest.
`remove` removes some duration, returns what's left.
They both ship with Euterpea

> cut' :: Dur -> Music a -> Music a
> cut' d m | d <= 0 = rest 0 -- note the "given" notation
> cut' d (Prim (Note oldD p)) = note (min oldD d) p
> cut' d (Prim (Rest oldD))   = rest (min oldD d)
> cut' d (m1 :=: m2)          = cut' d m1 :=: cut' d m2
> cut' d (m1 :+: m2)          = let m'1 = cut' d m1
>                                   m'2 = cut' (d - dur m'1) m2
>                               in m'1 :+: m'2
> cut' d (Modify (Tempo r) m) = tempo r (cut' (d*r) m)
> cut' d (Modify c m)         = Modify c (cut' d m)

Note the case of sequences, with two cases:
1. if `d` is greater than `dur m1`, then we return all of m1, followed by
   `d - dur m1` beats of m2.
2. if d is less than `dur m1`, then we return d beats of m1, followed by nothing.
   This works even if one of m1 or m2 is infinite.

As for remove:

> remove' :: Dur -> Music a -> Music a
> remove' d m | d <= 0 = m -- too short, return all of m
> remove' d (Prim (Note oldD p)) = note (max (oldD - d) 0) p
> remove' d (Prim (Rest oldD))   = rest (max (oldD - d) 0)
> remove' d (m1 :=: m2)          = remove' d m1 :=: remove' d m2
> remove' d (m1 :+: m2)          = let m'1 = remove' d m1
>                                      m'2 = remove' (d - dur m1) m2
>                                  in m'1 :+: m'2
> remove' d (Modify (Tempo r) m) = tempo r (remove' (d*r) m)
> remove' d (Modify c m)         = Modify c (remove' d m)

Note how the same two cases above are sorta equivalent:
1. If d is greater than `dur m1`, then we drop m1 altogether and then drop `d - dur m1` from m2
2. if d is less than `dur m1`, then we return all of m1, and all of m2 (since `d - dur m1` will be negative and the base case returns all)

Which can then be used:

λ> cut hn motif
Prim (Note (1 % 2) (D,4)) :+: Prim (Rest (0 % 1))
λ> cut' hn motif
Prim (Note (1 % 2) (D,4)) :+: Prim (Rest (0 % 1))
λ> remove hn motif
Prim (Note (0 % 1) (D,4)) :+: (Prim (Note (1 % 2) (A,4)) :+: (Prim (Note (1 % 2) (F,4)) :+: (Prim (Note (1 % 2) (D,4)) :+: (Prim (Note (1 % 2) (Cs,4)) :+: (Prim (Note (1 % 4) (D,4)) :+: (Prim (Note (1 % 4) (E,4)) :+: (Prim (Note (5 % 8) (F,4)) :+: (Prim (Note (1 % 8) (G,4)) :+: (Prim (Note (1 % 8) (F,4)) :+: (Prim (Note (1 % 8) (E,4)) :+: (Prim (Note (1 % 4) (D,4)) :+: Prim (Rest (0 % 1)))))))))))))
λ> remove' hn motif
Prim (Note (0 % 1) (D,4)) :+: (Prim (Note (1 % 2) (A,4)) :+: (Prim (Note (1 % 2) (F,4)) :+: (Prim (Note (1 % 2) (D,4)) :+: (Prim (Note (1 % 2) (Cs,4)) :+: (Prim (Note (1 % 4) (D,4)) :+: (Prim (Note (1 % 4) (E,4)) :+: (Prim (Note (5 % 8) (F,4)) :+: (Prim (Note (1 % 8) (G,4)) :+: (Prim (Note (1 % 8) (F,4)) :+: (Prim (Note (1 % 8) (E,4)) :+: (Prim (Note (1 % 4) (D,4)) :+: Prim (Rest (0 % 1)))))))))))))

Note that my current synthesizer (SimpleSynth), plays zero-duration notes as notes
with a very small duration, but it still plays them, thus:

6.6 Removing Zeros
==================

When playing, zeroes are unobtrusive; 
