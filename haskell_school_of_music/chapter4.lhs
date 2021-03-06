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


4.4 Simple Algorithmic Composition
----------------------------------

A simple algorithmic technique is introduced: given a simple melody,
we find all proper prefixes and then play them in two voices, one the reverse of
the other, played in parallel, then transposed a perfect fourth, then repeat.

> prefixes :: [a] -> [[a]]
> prefixes [] = []
> prefixes (x:xs) = let f pf = x:pf
>                   in [x] : map f (prefixes xs)

e.g.
λ> prefixes [1,2,3,4]
[[1],[1,2],[1,2,3],[1,2,3,4]]

> prefix :: [Music a] -> Music a
> prefix mel = let m1 = line (concat (prefixes mel))
>                  m2 = transpose 12 (line (concat (prefixes (reverse mel))))
>                  m  = instrument Flute m1 :=: instrument VoiceOohs m2
>              in m :+: transpose 5 m :+: m

Example melodies:

> mel1 = [c 5 en, e 5 sn, g 5 en, b 5 sn, a 5 en, f 5 sn, d 5 en, b 4 sn, c 5 en]
> mel2 = [c 5 sn, e 5 sn, g 5 sn, b 5 sn, a 5 sn, f 5 sn, d 5 sn, b 4 sn, c 5 sn]


Exercise: transcribe a song by a favorite composer

Bach's Sinfonia 15 in B minor
https://imslp.org/wiki/Sinfonia_in_B_minor,_BWV_801_(Bach,_Johann_Sebastian)
(a fantastic rendition: https://www.youtube.com/watch?v=Ya1m3kI2YM0)

Note that the numerator in the second term in the `t` value
is a good control of speed: less than the default of 120 makes it slower, e.g.
The 9/16 in the first term is simply from the score.

Note that all instances C and F are meant to be sharp, as this is in the
key of B Minor

some auxiliary functions (uses list comprehensions, introduced in the next chapter):

> pedal :: Int -> Music a -> [Dur -> Music a] -> Music a
> pedal t (Prim (Note d p)) ms = line [ m d :+: times 2 (note d p) | m <- ms ]
> pedal2 = pedal 2
> walk :: Dur -> [Dur -> Music a] -> Music a
> walk d (a:b:c:_) = addDur d [a,b,c,b,a]
> pedal' :: Int -> Music a -> [Dur -> Music a] -> Music a
> pedal' t pn ms = (times t pn) :+: pedal t pn ms -- start with pedal
> pedal'2 = pedal' 2
> intersperse :: Music a -> [Dur -> Music a] -> Music a
> intersperse (Prim (Note d p)) ms = line [ (note d p) :+: m d | m <- ms ]
> cheapTrill :: Music Pitch -> Music Pitch
> cheapTrill (Prim (Note d p)) =
>   note (d/5) p :+: note (d/5) (trans 1 p) :+:
>   note (d/5) p :+: note (d/5) (trans 1 p) :+:
>   note (d/5) p

> sinfonia15 :: Music Pitch
> sinfonia15 = let t = (9/16) * (140/120)
>              in instrument Harpsichord
>                 (tempo t (bass :=: treble))
> treble = trebl1  :+: (trebl2 :=: trebl3) :+:
>          trebl4  :+: (trebl5 :=: trebl6) :+:
>          trebl7  :+: (trebl8 :=: trebl9) :+:
>          trebl10 :+:
>          (trebl11 :=: trebl12) :+: trebl13 :+:
>          (trebl14 :=: trebl15)
> bass   = bas1 :+:
>          bas2 :+: (bas3 :=: bas4) :+:
>          bas5 :+: (bas6 :=: bas7) :+:
>          bas8 :+:
>          bas9
> trebl1 = addDur sn [b 4, fs 4, fs 4,
>                      g 4, fs 4, fs 4,
>                      b 4, fs 4, fs 4,
>                      cs 5, fs 4, fs 4,
>                      g 5, fs 4, fs 4,
>                      cs 5, fs 4, fs 4] :+: -- bars 1-2
>          addDur tn [d 5,
>                     b 4, d 5, fs 5, a 5,
>                     fs 5,
>                     d 5, fs 5,
>                     d 5, b 4, d 5, b 4,
>                     fs 4, b 4, fs 4, d 4, fs 4, d 4] :+: -- bar 3
>          b 3 sn :+: times 3 snr :+: addDur sn [b 4, cs 5, d 5, cs 5, b 4] :+: -- bar 4
>          as 4 (den + sn) :+: addDur sn [b 4, cs 5, b 4, cs 5, as 4] :+: -- bar 5
>          addDur tn [b 4, fs 4, b 4, d 5, f 5, d 5,
>                     b 4, d 5, b 4, fs 4, b 4, fs 4,
>                     d 4, fs 4, d 4, b 3, d 4, b 4] -- bar 6
> trebl3 = d 5 (dqn + den + sn) :+: -- bar 7, 8.25
>          addDur sn [b 4, b 4,
>                     cs 5, b 4, b 4,
>                     e  5, b 4, b 4] :+: -- bar 8
>          cs 5 (dqn + den + sn) :+: -- bar 9, 10.25
>          addDur sn [a 4, a 4,
>                     b 4, a 4, a 4,
>                     d 5, a 4, a 4] :+: -- bar 10
>          addDur tn [b 4, d 5, b 4, g 4, b 4, g 4] :+:
>          e 4 den :+:
>          addDur tn [e 5, g 5, e 5, cs 5, e 5, cs 5] -- bar 11
> trebl2 = addDur sn [b 4, fs 4, fs 4,
>                     g 4, fs 4, fs 4,
>                     b 4, fs 4, fs 4] :+: -- bar 7
>          gs 4 (dqn + den) :+: -- bar 8
>          addDur sn [a 4, e 4, e 4,
>                     fs 4, e 4, e 4,
>                     a 4, e 4, e 4] :+: -- bar 9
>          fs 4 (dqn + den) :+: -- bar 10
>          g 4 sn :+: snr :+: snr :+: denr :+: denr -- bar 11
> trebl4 = addDur tn [a 4, cs 5, a 4, fs 4, a 4, fs 4] :+:
>          d 4 den :+:
>          addDur tn [d 5, fs 5, d 5, b 4, d 5, b 4] :+: -- bar 12
>          addDur tn [g 4, b 4, g 4, e 4, g 4, e 4,
>                     cs 4, e 4, cs 4, a 3, cs 4, a 3] :+:
>          addDur sn [a 4, fs 4, g 4] :+: -- bar 13
>          (fs 4 den :=: d 4 sn)
> bas1 = b 2 den :+: snr :+:
>        addDur sn [b 3, cs 4, d 4, cs 4, b 3] :+:
>        as 3 (den + sn) :+: -- bars 1-2
>        addDur sn [b 3, cs 4, b 3, cs 4, as 3] :+:
>        b 3 den :+: denr :+: denr :+: -- bar 3
>        addDur sn [b 3, fs 3, fs 3,
>                   g 3, fs 3, fs 3,
>                   b 3, fs 3, fs 3,
>                   cs 3, fs 3, fs 3,
>                   g 3, fs 3, fs 3,
>                   cs 3, fs 3, fs 3] :+: -- bars 4-5
>        addDur tn [d 4, b 3, d 4, fs 4, b 4, d 5,
>                   d 4, fs 4, d 4, b 3, fs 4, b 3,
>                   fs 3, b 3, fs 3, d 3, fs 3, d 3] :+: -- bar 6
>        b 2 den :+: snr :+: addDur sn [b 3, cs 4, d 4, cs 4, b 3] :+: -- bar 7
>        e 4 den :+: snr :+: addDur sn [e 3, fs 3, gs 3, fs 3, e 3] :+: -- bar 8
>        a 3 den :+: snr :+: addDur sn [a 2, b 2, cs 3, b 2, a 2] :+: -- bar 9
>        d 3 den :+: snr :+: addDur sn [d 2, e 2, fs 2, e 2, d 2] :+: -- bar 10
>        g 2 den :+:
>        addDur tn [g 3, b 3, g 3, e 3, g 3, e 3,
>                   cs 3, e 3, cs 3, a 2, cs 3, a 2] :+: -- bar 11
>        fs 2 den :+: addDur tn [fs 3, a 3, fs 3, d 3, fs 3, d 3,
>                                b 2, d 3, b 2, g 2, b 2, g 2] :+: -- bar 12
>        e 2 den  :+: addDur tn [e 3, g 3, e 3, cs 3, e 3, cs 3,
>                                a 2, cs 3, a 2, fs 2, a 2, fs 2] -- bar 13
> ---- Defining them more closely together since bar 14:
> trebl6 = fs 4 den :+: denr :+: denr :+: -- bar 14
>          times 3 (denr) :+: -- bar 15 (the score has it as wnr
>          times 3 (denr) -- bar 16     but it was delayed too long)
> trebl5 = pedal2 (a 3 sn) [d 4, b 3, d 4] :+: -- bar 14
>          pedal2 (a 3 sn) [e 4, b 3, e 4] :+: -- bar 15
>          addDur tn [fs 4, d 4, a 4, d 5, b 4,
>                     gs 4, b 4, gs 4, e 4, gs 4, e 4] :+:
>          addDur tn [b 3, e 4, b 3, gs 3, b 3, gs 3] -- bar 16
> bas2 = d 2 den :+: snr :+: walk sn [d 2, e 2, f 2] :+: -- bar 14
>        cs 2 den :+: snr :+: walk sn [a 2, b 2, cs 3] :+: -- bar 15
>        d 3 (den + sn) :+: addDur sn [e 3, fs 3, e 3, fs 3, gs 3, d 3] -- bar 16
> trebl7 = pedal2 (e 4 sn) [a 4, fs 4, a 4] :+: -- bar 17
>          pedal2 (e 4 sn) [b 4, fs 4, b 4] :+: -- bar 18
>          addDur tn [cs 5, a 4, cs 5, e 5, a 5, fs 5,
>                     ds 5, fs 5, ds 5, b 4, ds 5, b 4,
>                     fs 4, b 4, fs 4, ds 4, fs 4, ds 4] -- bar 19
> bas4 = e 3 den :+: snr :+: walk sn [a 3, b 3, cs 4] :+: -- bar 17
>        gs 3 (den + sn) :+: addDur sn [a 3, b 3, a 3, b 3, gs 3] :+: -- bar 18
>        a 3  (den + sn) :+: addDur sn [b 3, c 4, b 3, c 4, a 3] -- bar 19
> bas3 = cs 3 den :+: denr :+: denr :+: -- bar 17
>        e  3 den :+: denr :+: denr :+: --bar 18
>        a  2 den :+: fs 3 dqn -- bar 19
> --- something may be weird with the times starting in bar 20?
> trebl9 = e 4 den :+: denr :+: g 4 (den + sn) :+: -- bar 20
>          pedal'2 (e 4 sn) [fs 4, a 4] :+: -- bar 21
>          fs 4 (dqn + den + sn) :+: -- bar 22 - 23.025
>          pedal'2 (d 4 sn) [e 4, g 4] :+: -- bar 23
>          e 4 (dqn + den + sn) -- bar 24
> trebl8 = pedal2 (b 3 sn) [e 4, c 4, e 4] :+: -- bar 20
>          cs 4 (dqn + den + sn) :+: -- bar 21, 22.025
>          pedal'2 (a 3 sn) [b 3, d 4] :+: -- bar 22
>          b  3 (dqn + den + sn) :+: -- bar 23,24.025
>          pedal'2 (gs 3 sn) [a 3, c 4] -- bar 24
> bas5   = g 3 den :+: snr :+: walk sn [e 3, fs 3, g 3] :+: -- bar 20
>          a 3 den :+: snr :+: walk sn [a 2, b 2, cs 3] :+: -- bar 21
>          d 3 den :+: snr :+: walk sn [d 3, e 3, fs 3] :+: -- bar 22
>          g 3 den :+: snr :+: walk sn [g 3, fs 3, e 3] :+: -- bar 23
>          cs 3 den :+: snr :+: walk sn [cs 3, b 2, as 2] -- bar 24
> -- final stretch: bar 26 may be suffering from insufficient
> -- silences in the bass line: seems to not wait enough?
> trebl10 = pedal'2 (cs 4 sn) [d 4, fs 4] :+: -- bar 25
>           intersperse (d 4 tn) [b 3, fs 4, fs 4] :+:
>           intersperse (b 4 tn) [fs 4, d 5, d  5] :+: 
>           (g 5 den) :+: -- bar 26
>           intersperse (cs 4 tn) [a 3, e 4,  d 4] :+:
>           intersperse (a  4 tn) [e 4, cs 5, cs 5] :+:
>           (fs 5 den) :+: -- bar 27
>           intersperse (b 3 tn)  [g 3, d 4, d 4] :+:
>           intersperse (g 4 tn)  [d 4, b 4, b 4] :+:
>           (e 5 (den + sn)) :+: -- bar 28
>           pedal'2 (fs 4 sn) [g 4, e 5] -- bar 29
> bas7    = as 3 (dqn + den) :+: -- bar 25
>           b 3 sn -- bar 26
> bas6    = (fs 2 den) :+: snr :+:
>           walk sn [fs 3, e 3, d 3] :+: -- bar 25
>           b 2 den :+:
>           intersperse (d 3 tn) [b 2, fs 3, fs 3] :+:
>           intersperse (b 3 tn) [g 3, e 4, e 4] :+: -- bar 26
>           a 4 den :+:
>           intersperse (cs 3 tn) [a 2, e 3, e 3] :+:
>           intersperse (a  3 tn) [fs 3, d 4, d 4] :+: -- bar 27
>           intersperse (g  4 tn) [b 4, d 4, d 4] :+:
>           intersperse (b  3 tn) [d 4, g 3, g 3] :+:
>           intersperse (e  3 tn) [g 3, cs 3, cs 3] :+: -- bar 28
>           as 2 (den + sn) :+:
>           addDur sn [b 2, cs 3, fs 2, b 2, as 2] -- bar 29
> trebl12 = d 5 (dqn + den + sn) :+: -- bar 30
>           pedal'2 (b 4 sn) [c 5, g 5] :+: -- bar 31
>           as 4 den :+: fs 5 (dqn + tn) -- bar 32
> trebl11 = pedal2 (b 3 sn) [f 4, d 4, f 4] :+: -- bar 30
>           g 4 (dqn + den) :+: -- bar 31
>           fs 4 den :+: as 4 dqn -- bar 32
> trebl13 = addDur tn [b 4, d 5, fs 5, b 5, fs 5] :+:
>           intersperse (d 5 tn) [fs 5, b 4, b 4] :+:
>           intersperse (fs 4 tn) [b 4, d 4, d 4] -- bar 33
> bas8    = b 2 den :+: denr :+: b 2 den :+: -- bar 30
>           e 3 (dqn + den + den + dqn) :+: -- bars 31-32
>           d 3 en :+: snr :+: denr :+: denr -- bar 33
> trebl15 = d 5 (dqn + den + sn) :+:  -- bar 34
>           pedal'2 (cs 5 sn) [gs 5, d 5] :+: -- bar 35
>           addDur sn [e 5, cs 5, cs 5] :+:
>           as 5 sn :+: walk sn [cs 5, d 5, e 5] :+: -- bar 36
>           addDur sn [fs 5, g 5, e 5] :+:
>           d 5 den :+: cheapTrill (cs 5 en) :+:
>           b 4 sn :+: -- bar 37
>           b 4 dhn -- bar 38, FIN
> trebl14 = e 4 (dqn + den + dqn + den) :+: -- bars 34-35
>           as 4 den :+: snr :+:
>           walk sn [as 4, b 4, cs 5] :+: -- bar 36
>           b 4 dqn :+: as 4 den :+: -- bar 37
>           b 4 dhn -- bar 38, FIN
> bas9    = addDur tn [b 3, g 3, b 3, d 4, g 4, d 4] :+:
>           intersperse (b 3 tn) [d 4, g 3, g 3] :+:
>           intersperse (d 3 tn) [g 3, b 2, b 2] :+: -- bar 34
>           es 2 den :+: denr :+: denr :+: -- bar 35
>           fs 2 den :+: snr :+:
>           walk sn [e 3, fs 3, g 3] :+: -- bar 36
>           d 3 en :+: e 3 sn :+:
>           fs 3 en :+: e 3 sn :+:
>           fs 3 en :+: fs 2 sn :+: -- bar 37
>           b 2 dhn -- bar 38, FIN


Another fun tune, perhaps for the presentation:
https://imslp.org/wiki/Toccata_and_Fugue_in_D_minor%2C_BWV_565_(Bach%2C_Johann_Sebastian)
Bach's tocatta and fugue in d-moll: https://www.youtube.com/watch?v=ho9rZjlsyYY
only the first few bars, however:


> mordent' :: Int -> Music Pitch -> Music Pitch
> mordent' n (Prim (Note d p)) =
>   (note (d/16) p) :+: (note (d/16) (trans n p)) :+: (note (14 * d/16) p)
> invMordent :: Music Pitch -> Music Pitch
> invMordent = mordent' (-1)
> mordent :: Music Pitch -> Music Pitch
> mordent = mordent' 1
> wait :: Int -> Dur -> Music Pitch -> Music Pitch
> wait n d p = (times n (rest d)) :+: p

> tocattaDMoll :: Music Pitch
> tocattaDMoll = let t = (4/4) * (20/120)
>                in instrument StringEnsemble1
>                   (tempo t ((manuale :=: pedale) :=:
>                             (manuale :=: pedale)))
> manuale = voice1a :=: voice2a 
> pedale = voice3a :+: 
>          voice3b :=: voice4a :+: (bigChord :=: coda)
> voice1a = invMordent (a 5 en) :+: tnr :+:
>           addDur sfn [g 5, f 5, e 5, d 5] :+: cs 5 tn :+:
>           d 5 sn
> voice2a = invMordent (a 4 en) :+: tnr :+:
>           addDur sfn [g 4, f 4, e 4, d 4] :+: cs 4 tn :+:
>           d 4 sn :+:
>           invMordent (a 4 en) :+: tnr :+:
>           addDur tn [e 4, f 4, cs 4, d 4] :+: rest (tn + sn)
> voice3a = rest (en + tn + sfn + sfn + sfn + sfn + tn + sn) :+:
>           invMordent (a 3 en) :+: tnr :+:
>           addDur tn [e 3, f 3, cs 3, d 3] :+: rest (tn + sn)
> voice3b = invMordent (a 3 en) :+: tnr :+:
>           addDur sfn [g 3, f 3, e 3, d 3] :+: cs 3 tn :+:
>           d 3 sn :+: rest (tn + sn)
> voice4a = invMordent (a 2 en) :+: tnr :+:
>           addDur sfn [g 2, f 2, e 2, d 2] :+: cs 2 tn :+:
>           d 2 sn :+: rest (tn + sn)
> bigChord = chord [c0, c1, c2, c3, c4, c5, c6]
> c0 = wait 0 sn (d 2 hn)
> c1 = wait 1 sn (cs 3  (qn + qn))
> c2 = (wait 2 sn (e  3 (qn + qn)))
> c3 = (wait 3 sn (g  3 (qn + qn + sn)))
> c4 = (wait 4 sn (b  3 (qn + qn)))
> c5 = (wait 5 sn (cs 4 (qn + qn))) 
> c6 = (wait 6 sn (e  4 (qn + qn)))
> coda = wait 6 sn (rest (qn + qn)) :+:
>        (d 3 qn :=: a 3 qn :=: d 4 qn) :=:
>        wait 1 sn (e 3 sn :+: fs 3 en)

