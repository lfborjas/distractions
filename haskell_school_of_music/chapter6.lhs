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

When playing, zeroes are unobtrusive; but they're annoying when reading.
Euterpea ships with `removeZeros`:

> removeZeros' :: Music a -> Music a
> removeZeros' (Prim p) = Prim p
> removeZeros' (m1 :+: m2) =
>   let m'1 = removeZeros' m1
>       m'2 = removeZeros' m2
>   in case (m'1, m'2) of
>     (Prim (Note 0 p), m) -> m
>     (Prim (Rest 0), m)   -> m
>     (m, Prim (Note 0 p)) -> m
>     (m, Prim (Rest 0))        -> m
>     (m1, m2)             -> m1 :+: m2
> removeZeros' (m1 :=: m2) =
>   let m'1 = removeZeros' m1
>       m'2 = removeZeros' m2
>   in case (m'1, m'2) of -- `case` only matches against one value, but we can use a tuple to inspect more than one value.
>     (Prim (Note 0 p), m) -> m
>     (Prim (Rest 0), m)   -> m
>     (m, Prim (Note 0 p)) -> m
>     (m, Prim (Rest 0))        -> m
>     (m1, m2)             -> m1 :=: m2
> removeZeros' (Modify c m) = Modify c (removeZeros' m)

Which produces a better remove/cut than the ones with zero-duration notes (which
uncovers a bug in my synthesizer, as explained above):

λ> play $ removeZeros'  $ remove' wn motif
λ> play $ remove' wn motif

λ> remove' (wn+wn) motif
Prim (Note (0 % 1) (D,4)) :+: (Prim (Note (0 % 1) (A,4)) :+: (Prim (Note (0 % 1) (F,4)) :+: (Prim (Note (0 % 1) (D,4)) :+: (Prim (Note (1 % 2) (Cs,4)) :+: (Prim (Note (1 % 4) (D,4)) :+: (Prim (Note (1 % 4) (E,4)) :+: (Prim (Note (5 % 8) (F,4)) :+: (Prim (Note (1 % 8) (G,4)) :+: (Prim (Note (1 % 8) (F,4)) :+: (Prim (Note (1 % 8) (E,4)) :+: (Prim (Note (1 % 4) (D,4)) :+: Prim (Rest (0 % 1)))))))))))))
λ> removeZeros' $ remove' (wn+wn) motif
Prim (Note (1 % 2) (Cs,4)) :+: (Prim (Note (1 % 4) (D,4)) :+: (Prim (Note (1 % 4) (E,4)) :+: (Prim (Note (5 % 8) (F,4)) :+: (Prim (Note (1 % 8) (G,4)) :+: (Prim (Note (1 % 8) (F,4)) :+: (Prim (Note (1 % 8) (E,4)) :+: Prim (Note (1 % 4) (D,4))))))))

6.7 Truncating Parallel Composition
====================================

The duration of m1 :=: m2 is the same as the longer of the music values. But what
if we wanted a parallel that lasted the `shorter` of them?
It's not as easy as it sounds, as we may truncate a sequence in the middle of a note,
or the music values may be infinite.

A first attempt could be:

(/=:) :: Music a -> Music a -> Music a
m1 /=: m2 = cut (dur m2) m1 :=: cut (dur m1) ms

Unfortunately, this can't handle infinite durations, since, unlike `cut`, we need
to compute both durations.

Lazy evaluation can help, by helping us express infinite durations as a list
of durations where the last element is the actual duration.

type LazyDur' = [Dur]


Now we can define a specialized version of Dur (which ships with Euterpea):

> durL' :: Music a -> LazyDur
> durL' m@(Prim _)  = [dur' m]
> durL' (m1 :+: m2) = let d1 = durL' m1
>                     in d1 ++ map (+(last d1)) (durL' m2)
> durL' (m1 :=: m2) = mergeLD' (durL' m1) (durL' m2)
> durL' (Modify (Tempo r) m) = map (/r) (durL' m)
> durL' (Modify _ m)         = durL' m

Where mergeLD merges two lazy durations into one:

> mergeLD' :: LazyDur -> LazyDur -> LazyDur
> mergeLD' [] ld = ld
> mergeLD' ld [] = ld
> mergeLD' ld1@(d1:ds1) ld2@(d2:ds2) =
>   if d1 < d2 then d1 : mergeLD' ds1 ld2
>   else d2 : mergeLD' ld1 ds2


Which means we can now approximate the duration of an infinite sequence:

λ> dur motif
17 % 4
λ> take 20 $ durL $ motif
[1 % 2,1 % 1,3 % 2,2 % 1,5 % 2,11 % 4,3 % 1,29 % 8,15 % 4,31 % 8,4 % 1,17 % 4,17 % 4]
λ> take 20 $ durL $ forever motif
[1 % 2,1 % 1,3 % 2,2 % 1,5 % 2,11 % 4,3 % 1,29 % 8,15 % 4,31 % 8,4 % 1,17 % 4,17 % 4,19 % 4,21 % 4,23 % 4,25 % 4,27 % 4,7 % 1,29 % 4]


And minL' compares two durations:

> minL' :: LazyDur -> Dur -> Dur
> minL' [] d'     = 0
> minL' [d] d'    = min d d'
> minL' (d:ds) d' = if d < d' then minL' ds d' else d'

Which, finally, yields a new version of cut:


> cutL' :: LazyDur -> Music a -> Music a
> cutL' [] m  = rest 0
> cutL' (d:ds) m | d <= 0 = cutL' ds m
> cutL' ld (Prim (Note oldD p)) = note (minL' ld oldD) p
> cutL' ld (Prim (Rest oldD))   = rest (minL' ld oldD)
> cutL' ld (m1 :=: m2)          = cutL' ld m1 :=: cutL' ld m2
> cutL' ld (m1 :+: m2)          = let m'1 = cutL' ld m1
>                                     m'2 = cutL'
>                                           (map (\d -> d - dur m'1) ld) m2
>                               in m'1 :+: m'2
> cutL' ld (Modify (Tempo r) m) = tempo r (cutL' (map (*r) ld) m)
> cutL' ld (Modify c m)         = Modify c (cutL' ld m)

Note that cutL is very similar to cut, it just is aware of lazy durations.

Now, we can define the operator:

(/=:) :: Music a -> Music a -> Music a

Which, to not clash with Euterpea, we'll modify a bit:

> (/=:/) :: Music a -> Music a -> Music a
> m1 /=:/ m2 = cutL' (durL' m2) m1 :=: cutL' (durL' m1) m2


And now we use it: note that it _won't_ play that last tritone:

λ> cut hn $ (forever $ c 4 qn :+: d 4 qn :+: e 4 qn) /=:/ (forever $ e 4 qn :+: fs 4 qn :+: fs 4 qn)
((Prim (Note (1 % 4) (C,4)) :+: (Prim (Note (1 % 4) (D,4)) :+: Prim (Rest (0 % 1)))) :+: Prim (Rest (0 % 1))) :=: ((Prim (Note (1 % 4) (E,4)) :+: (Prim (Note (1 % 4) (Fs,4)) :+: Prim (Rest (0 % 1)))) :+: Prim (Rest (0 % 1)))
λ> play $ cut hn $ (forever $ c 4 qn :+: d 4 qn :+: e 4 qn) /=:/ (forever $ e 4 qn :+: fs 4 qn :+: fs 4 qn)

Unless we, of course, tell it to:

λ> cut dhn $ (forever $ c 4 qn :+: d 4 qn :+: e 4 qn) /=:/ (forever $ e 4 qn :+: fs 4 qn :+: fs 4 qn)
((Prim (Note (1 % 4) (C,4)) :+: (Prim (Note (1 % 4) (D,4)) :+: Prim (Note (1 % 4) (E,4)))) :+: Prim (Rest (0 % 1))) :=: ((Prim (Note (1 % 4) (E,4)) :+: (Prim (Note (1 % 4) (Fs,4)) :+: Prim (Note (1 % 4) (Fs,4)))) :+: Prim (Rest (0 % 1)))

Knowing all this cool stuff, let's try an infinite rising sequence:

> perpetuum :: Music Pitch -> [Music Pitch]
> perpetuum (Prim (Note d r)) =
>   [ note d p :=: note d (trans 4 p) | -- create a major third dyad
>     p <- map pitch $
>          [absPitch r, absPitch (trans 7 r)..] ] -- create an infinite sequence of fifths

Note that:

λ> map pitch $ take 12 [absPitch (C,4), absPitch (trans 7 (C,4))..]
[(C,4),(G,4),(D,5),(A,5),(E,6),(B,6),(Fs,7),(Cs,8),(Gs,8),(Ds,9),(As,9),(F,10)]

Is the circle of fifths.

6.8 Trills
==========

We'll define two approches to trills that will produce the ornament with a duration
equal to the original note:

These do _not_ ship with Euterpea!

> trill :: Int -> Dur -> Music Pitch -> Music Pitch
> trill i sDur (Prim (Note tDur p)) =
>   if sDur >= tDur then note tDur p
>   else note sDur p :+:
>        trill (negate i) sDur
>              (note (tDur - sDur) (trans i p))
> trill i d (Modify (Tempo r) m) = tempo r (trill i (d*r) m)
> trill i d (Modify c m) = Modify c (trill i d m)
> trill _ _ _ = error "trill: input must be a single note."

This function takes an interval for the trill (`i`), a duration for the trill note
(`sDur`) and the note to add a trill to.

Note that `negate`:

λ> :info negate
class Num a where
  ...
  negate :: a -> a
  ...
  	-- Defined in ‘GHC.Num’


We can also define a version that starts on the trill note instead of the original:
(by simply saying that the interval is what would bring us back to the original,
and that the new original is what would be the trill note):

> trill' :: Int -> Dur -> Music Pitch -> Music Pitch
> trill' i sDur m = trill (negate i) sDur (transpose i m)

Which looks like:

λ> trill 1 sn $ c 4 qn
Prim (Note (1 % 16) (C,4)) :+: (Prim (Note (1 % 16) (Cs,4)) :+: (Prim (Note (1 % 16) (C,4)) :+: Prim (Note (1 % 16) (Cs,4))))

λ> trill' 1 sn $ c 4 qn
Modify (Transpose 1) (Prim (Note (1 % 16) (C,4)) :+: (Prim (Note (1 % 16) (B,3)) :+: (Prim (Note (1 % 16) (C,4)) :+: Prim (Note (1 % 16) (B,3)))))

And the second way to define the trill, is in terms of the number of subdivided notes
to be included in the trill:

> trilln :: Int -> Int -> Music Pitch -> Music Pitch
> trilln i nTimes m = trill i (dur m/fromIntegral nTimes) m

Which is another useful way of thinking about trills:

λ> trilln 1 4 $ c 4 hn
Prim (Note (1 % 8) (C,4)) :+: (Prim (Note (1 % 8) (Cs,4)) :+: (Prim (Note (1 % 8) (C,4)) :+: Prim (Note (1 % 8) (Cs,4))))

And this too can be made to start on the other note:

> trilln' :: Int -> Int -> Music Pitch -> Music Pitch
> trilln' i nTimes m = trilln  (negate i) nTimes (transpose i m)

λ> trilln' 1 4 $ c 4 hn
Modify (Transpose 1) (Prim (Note (1 % 8) (C,4)) :+: (Prim (Note (1 % 8) (B,3)) :+: (Prim (Note (1 % 8) (C,4)) :+: Prim (Note (1 % 8) (B,3)))))

And a `roll` is a trill with a zero interval--useful for percussion:

> roll :: Dur -> Music Pitch -> Music Pitch
> rolln :: Int -> Music Pitch -> Music Pitch
>
> roll dur m = trill 0 dur m
> rolln nTimes m = trilln 0 nTimes m

λ> rolln 4 $ c 4 hn
Prim (Note (1 % 8) (C,4)) :+: (Prim (Note (1 % 8) (C,4)) :+: (Prim (Note (1 % 8) (C,4)) :+: Prim (Note (1 % 8) (C,4))))

6.9 Grace Notes
===============

This is a more general function of our graceNote fn, taking a rational that specifies
the fraction of the principal note's duration to use for the grace note:

> grace :: Int -> Rational -> Music Pitch -> Music Pitch
> grace n r (Prim (Note d p)) =
>   note (r*d) (trans n p) :+: note ((1-r)*d) p
> grace n r _ =
>   error "grace: can only add a grace note to a note"

Note that we multiply by a fraction, effectively having a shorter duration:

λ> grace (-1) (1/8) $ c 4 qn
Prim (Note (1 % 32) (B,3)) :+: Prim (Note (7 % 32) (C,4))

Note that this assumes that the grace note can be in the downbeat of the principal
note. Sometimes, we want it to encroach upon the beat of the _previous_ note:

> grace2 :: Int -> Rational -> Music Pitch -> Music Pitch -> Music Pitch
> grace2 n r (Prim (Note d1 p1)) (Prim (Note d2 p2)) =
>   note (d1 - r * d2) p1 :+: note (r*d2) (trans n p2) :+: note d2 p2
> grace2 _ _ _ _ =
>   error "grace2: can only add a grace note to a note."

λ> grace2 (-1) (1/8) (d 4 hn) (c 4 qn)
Prim (Note (15 % 32) (D,4)) :+: (Prim (Note (1 % 32) (B,3)) :+: Prim (Note (1 % 4) (C,4)))

Note how it steals from the previous note, and leaves the principal unchanged:

λ> 15/32 + 1/32
0.5
λ> hn
1 % 2 -- i.e. the previous note + the grace note last as long as the origin previous note.

Exercise 6.6: define mordent, turn and appogiatura:

> mordent :: Int -> Rational -> Music Pitch -> Music Pitch
> mordent n r (Prim (Note d p)) =
>   note (r*d/2) p :+:
>   note (r*d/2) (trans n p) :+:
>   note (1-(r*d/2 + r*d/2)) p
> 
