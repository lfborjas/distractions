Chapter 4: a musical interlude

> module Chapter4 where
> import Euterpea

This chapter gives a glimpse into writing a module to represent a particular
piece of music.

This is all provided by the book's author: https://github.com/Euterpea/HSoM/blob/b97f6ee0ae7e09ce3200a5d662418af580f05ea5/HSoM/Examples/Interlude.lhs

4.1 Transcribing an existing score
==================================

Transcribes 'twinkle twinkle little star', first using a rather verbose approach,
note for note:

twinkle =
  c 4 qn :+: c 4 qn ...

But then pointing out there's enough repetition to come up with some functions:

> pcToQn :: PitchClass -> Music Pitch
> pcToQn pc = note qn (pc, 4)

With this, we reduce the melody to combine and reduce repetition:

> twinkle1 =
>   line (map pcToQn [C, C, G, G, A, A]) :+: g 4 hn :+:
>   line (map pcToQn [F, F, E, E, D, D]) :+: c 4 hn :+:
>   line (map pcToQn [G, G, F, F, E, E]) :+: d 4 hn :+:
>   line (map pcToQn [G, G, F, F, E, E]) :+: d 4 hn :+:
>   line (map pcToQn [C, C, G, G, A, A]) :+: g 4 hn :+:
>   line (map pcToQn [F, F, E, E, D, D]) :+: c 4 hn

Which shows even further patterns that can be refactored:

> twinkle2 =
>   let m1 = line (map pcToQn [C, C, G, G, A, A]) :+: g 4 hn
>       m2 = line (map pcToQn [F, F, E, E, D, D]) :+: c 4 hn
>       m3 = line (map pcToQn [G, G, F, F, E, E]) :+: d 4 hn
>   in line [m1, m2, m3, m3, m1, m2]

Futher refinements can be done, but:

  that which is maximally concise is not necessarily maximally readable, or
  maximally editable

4.2 Modules
===========

Modules usually group common types and functions, first, the header:

module Interlude where
import Euterpea

Module names must always be capitalized, and they can follow a hierarchy:

module HSoM.Examples.Interlude where

Where segments in the hierarchy correspond to folders, and the last segment
is the filename.

One can also choose what is exposed (provided) by the module, so imports
don't bring in all the definitions:

modle HSoM.Examples.Interlude (childSong6, prefix) where
import Euterpea

In the example above, only `childSong6` and `prefix` are visible to the outside
world. This is often called the *export list* or *interface* of the module.

Explicit type signatures in export lists aren't allowed, but one can add comments:

module HSoM.Examples.Interlude
       (childSong6, -- :: Music Pitch
        ...
       ) where
import Euterpea

4.3 Transcribing a more complex score
=====================================

The first 28 bars of Chick Corea's "Children's Songs No. 6" is provided,
and it's pointed out that the bass line can be seen to consist
entirely of three repeating phrases. So, we define:

> times' :: Int -> Music a -> Music a
> times' 0 m = rest 0
> times' n m = m :+: times' (n - 1) m

(Notice the pattern matching applied to numbers).
N.B. `Euterpea` already defines `times`, hence the `'` indicator here.

It's also noticeable in the score that many melodic lines consist of a sequence
of consecutive notes having the same duration, so we define:

> addDur :: Dur -> [Dur -> Music a] -> Music a
> addDur d ns = let f n = n d
>               in line (map f ns)

Which takes advantage of the fact that we can curry note-building functions
(e.g. a 4 is a functio of type Dur -> Music a, that is, a note without yet a
duration).

Finally, we want to add grace notes; we assume that the grace note begins
on the downbeat of the principal note, and thus its duration subtracts from it.
We also assume that it's only 1/8th of the principal note. So we want:

> graceNote :: Int -> Music Pitch -> Music Pitch

Such that `graceNote n (note d p)` is a `Music` value consisting of two notes,
first the grace note of d/8 duration and a pitch n semitones offset from `p`,
and the second note, the principal, now has 7d/8 duration.

> graceNote n (Prim (Note d p)) =
>   note (d/8) (trans n p) :+: note (7 * d/8) p
> graceNote n _ =
>   error "Can only add a grace note to a note"

Notice that we needed to reach into the principal note to grab its duration, which
means that we're pattern-matching against its very constructor, not just
a variable! We can't pattern match against function application:

f (Prim (Note d p)) =

is perfectly fine, whereas:

f (note d p) =

isn't.

Notice also how, for thoroughness, if `graceNote` happens to not be applied to a
note, we raise a custom error.

A couple of other interesting adornments not addressed in this chapter:

- the single staccatto on note four of bar 15
- single portamento on note three of bar sixteen.

They will be addressed later.

Now we're ready to tackle the song!

4.3.2 Bass line
---------------

> b1 = addDur dqn [b 3,  fs 4, g 4,  fs 4]
> b2 = addDur dqn [b 3,  es 4, fs 4, es 4]
> b3 = addDur dqn [as 3, fs 4, g 4,  fs 4]

Now we can define the 28 bars of the bass line easily:

> bassLine = times 3 b1 :+: times 2 b2 :+:
>            times 4 b3 :+: times 5 b1

4.3.3 Main Voice
---------------

At the highest level, this voice consists of the phrase `v1`, and a longer
remainer, `v2`, with seven pieces.

> mainVoice = times 3 v1 :+: v2
> v1  = v1a :+: graceNote (-1) (d 5 qn) :+: v1b -- bars 1-2
> v1a = addDur en [a 5, e 5, d 5, fs 5, cs 5, b 4, e 5, b 4]
> v1b = addDur en [cs 5, b 4]
> v2  = v2a :+: v2b :+: v2c :+: v2d :+: v2e :+: v2f :+: v2g
> v2a = line [cs 5 (dhn + dhn), d 5 dhn, f 5 hn, gs 5 qn,
>             fs 5 (hn + en), g 5 en] -- bars 7-11
> v2b = addDur en [fs 5, e 5, cs 5, as 4] :+: a 4 dqn :+:
>       addDur en [as 4, cs 5, fs 5, e 5, fs 5] -- bars 12-13
> v2c = line [g 5 en, as 5 en, cs 6 (hn + en), d 6 en, cs 6 en] :+:
>       e 5 en :+: enr :+:
>       line [as 5 en, a 5 en, g 5 en, d 5 qn, c 5 en, cs 5 en] -- bars 14-16
> v2d = addDur en [fs 5, cs 5, e 5, cs 5,
>                  a 4, as 4, d 5, e 5, fs 5] -- bars 17-18.5
> v2e = line [graceNote 2 (e 5 qn), d 5 en, graceNote 2 (d 5 qn), cs 5 en,
>             graceNote 1 (cs 5 qn), b 4 (en + hn), cs 5 en, b 4 en] -- bars 18.5-20
> v2f = line [fs 5 en, a 5 en, b 5 (hn + qn), a 5 en, fs 5 en, e 5 qn,
>             d 5 en, fs 5 en, e 5 hn, d 5 hn, fs 5 qn] -- bars 21-23
> v2g = tempo (3/2) (line [cs 5 en, d 5 en, cs 5 en]) :+:
>       b 4 (3 * dhn + hn) -- bars 24-28

Some notes:

- For convenience, the division of phrases is made to line up with bar lines.
  This is not the only (or best) way to organize the music: for example,
  the last two notes of bar 20 more logically fall with the following phrase.
- The staccato is treated by playing the quarter note as an eighth note,
  and the later portamento is ignored.
- The triplet of eighth notes in bar 25 is done by scaling the tempo by a factor of
  3/2
- Notice that when the notes are linked, we express that in the code by
  explicitly summing their durations--reflecting the score vs. simplifying
  mathematically.

4.3.4 Putting it all together
-----------------------------

- Chick Corea noted that songs 1-15 were written for the Fender Rhodes, we have
  a `RhodesPiano` InstrumentName available.
- The score notes that a dotted half note has a metronome value of 69. By default,
  `play` uses a tempo equivalent to a quarter note with a metronome value of 120;
  therefore, the tempo should be scaled.

> childSong6 :: Music Pitch
> childSong6 = let t = (dhn/qn) * (69/120)
>              in instrument RhodesPiano
>                 (tempo t (bassLine :=: mainVoice))