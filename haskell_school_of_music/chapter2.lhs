Chapter 2: Simple Music

> module Chapter2 where
> import Euterpea

This chapter introduces a few fundamental musical concepts as abstracted by
Euterpea.

2.1 Preliminaries
=================

For example, there's a few type *synonyms*

type Octave = Int
type Pitch  = (PitchClass, Octave)
type Dur    = Rational -- Rationals are printed like numerator % denominator

Duration is a rational mostly because music tends to see durations in fractions
of notes, and irrational numbers are rarely needed.

This allows us to define some helpful variables, like a quarter note:

qn :: Dur
qn = 1/4

Then there's *algebraic data types*:

data PitchClass = Cff | Cf | C | Dff...

(enumerates 35 pitch class names, with all the enharmonics, any pitch class left
of another one is "lower than" the other).

A `data` declaration is a user-defined type.

2.2. Notes, Music and Polymorphism
==================================

A music "primitive" is the smallest unit that can be performed. These are notes and
rests. A first approach to defining them may be:

data Primitive = Note Dur Pitch
               | Rest Dur

Which says: "to obtain a primitive, you can either construct a Note or a Rest";
that is, we can think of the type constructors as taking arguments:

Note :: Dur -> Pitch -> Primitive
Rest :: Dur ->          Primitive

A note, however, doesn't have to be an audible pitch, and may have alterations
not reflected above. To open this up, we can redefine it as a *polymorphic data type*, that is, leaving the type of the pitch unspecified:

data Primitive a = Note Dur a
                 | Rest Dur

Where `a` is a *type variable*. This makes Note and Rest *polymorphic functions*:

Note :: Dur -> a -> Primitive a
Rest :: Dur ->      Primitive a

Next, is the most important definition, for the fundamental structure of a
note-level musical entity:

data Music a =
     Prim (Primitive a)       -- primitive value
   | Music a :+: Music a      -- sequential composition
   | Music a :=: Music a      -- parallel composition
   | Modify Control (Music a) -- modifier

Which makes the types of the constructors:

Prim   :: Primitive a ->        Music a
(:+:)  :: Music a -> Music a -> Music a
(:=:)  :: Music a -> Music a -> Music a
Modify :: Control -> Music a -> Music a

Note that the definition of `Music a` is recursive. These are called
*inductive data types*, to allow for all kinds of complexity.

For example:

> a440m :: Music Pitch
> a440m = Prim (Note qn (A, 4))

This works because Music Pitch can be constructed from Prim (Primitive a)
which in turn can be constructed from Prim (Note Dur a), where a = Pitch

Control is interesting: it can annotate a Music value with a tempo change,
a transposition, a phrase attribute, an instrument, a key signature
or a custom label:

data Control =
     Tempo Rational            -- scale the tempo
   | Transpose AbsPitch        -- transposition
   | Instrument InstrumentName -- instrument label
   | Phrase [PhraseAttribute]  -- phrase attributes
   | KeySig PitchClass Mode    -- key signature and mode
   | Custom String             -- custom label

For example, to play in an overdriven guitar, we could say:

> dGuitar :: Control
> dGuitar = (Instrument DistortionGuitar)
> dChord  :: Music Pitch
> dChord  =  (Prim (Note qn (D,3))) :=: (Prim (Note qn (F, 3)))
> guitarD :: Music Pitch
> guitarD = Modify dGuitar dChord

Mode, InstrumentName and others are defined in Euterpea:

https://github.com/Euterpea/Euterpea2/blob/bc89ee9a47fb06ef5908208609064789fab994a3/Euterpea/Music.lhs

Another interesting one is e.g. a phrase attribute:

> trilled :: Control
> trilled = Phrase [ Orn Trill ]
> trillGuitar :: Music Pitch
> trillGuitar = Modify dGuitar (Modify trilled dChord)

2.3 Convenient auxiliary functions
==================================

A bunch of interesting functions are defined by Euterpea, for example:

note :: Dur -> a -> Music a
note d p = Prim (Note d p)

rest :: Dur -> Music a
rest d = Prim (Rest d)

instrument :: InstrumentName -> Music a -> Music a
instrument i m = Modify (Instrument i) m

(and a few more visible in the above definition by Euterpea, and the book)

same for some common note and rest names as a shorthand for creating note values:

cff, cf, c, cs :: Octave -> Dur -> Music Pitch
cff o d = note d (Cff, o); cf o d = note d (Cf, 0)
...
wn, qn, ... :: Dur

which means the above can be written as:

> guitarD2 :: Music Pitch
> guitarD2 = instrument DistortionGuitar (d 3 qn :=: f 3 qn)

Or as the example the book gives, creating a ii-V-I chord progression
using triads in the key of C major:

> t251 :: Music Pitch
> t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
>            gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
>            cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
>        in dMinor :+: gMajor :+: cMajor

One note on `play`: the type of the argument must be clear,
play (note qn (C, 4)) would fail because Haskell can't
quite tell what type of number 4 is, we need to make it explicit with

m :: Music Pitch
m = note qn (C, 4)

Which resolves the mystery for Haskell, or we can declare the type "inline":

play (note qn ((C,4) :: Pitch))

Exercise 2.1:
-------------

Define a function twoFiveOne :: Pitch -> Dur -> Music Pitch
where `twoFiveOne p d` constructs a ii-V-I progression in the key
whose major scale begins on the pitch p, the duration of the first two chords
is `d` and the duration of the last chord is 2 * d
twoFiveOne (C, 4) wn should be equivalent to t251

In my solution, I use `trans` which adds semitones to the base note;
I wanted instead to use degrees, but not sure how that works yet!
c + 4 semitones = e
c + 4 + 3 semitones = g

> twoFiveOne :: Pitch -> Dur -> Music Pitch
> twoFiveOne p d = let root        = note d p
>                      thirdMinor  = note d (trans 3 p)
>                      thirdDegree = note d (trans 4 p)
>                      fifthDegree = note d (trans 7 p)
>                      majChord    = root :=: thirdDegree :=: fifthDegree
>                      minChord    = root :=: thirdMinor  :=: fifthDegree
>                      oneMaj      = majChord
>                      twoMin      = transpose 2 minChord
>                      fiveMaj     = transpose 7 majChord
>                  in  twoMin :+: fiveMaj :+: (tempo 2 oneMaj)

Note that I probably have many more variables than necessary (my first solution was
only about four lines); but it was fun to introduce more definitions to make
it more self-explanatory: the notes as transpositions, the chords as compositions
of notes or transpositions of other chords, the progression as the actual chords.

Exercise 2.2:
------------

PitchClass implies the use a 12-tone equal temperament scale. Let's
try to work with the pentatonic blues scale, which in the key of C is approximately
C, Eb, F, G, and Bb -- root, minor third, fourth, fifth, minor seventh.

1. Define a new algebraic data type called BluesPitchClass: Ro, MT, Fo, Fi, MS
2. Define a type synonym BluesPitch, akin to Pitch
3. Define the auxiliary functions ro, mt, fo, fi, ms to construct notes.
4. Define a `fromBlues` function (:: Music BluesPitch -> Music Pitch) to play
   using the approximate translation.
5. Write out a few melodies

> data BluesPitchClass = Ro | MT | Fo | Fi | MS
> type BluesPitch      = (BluesPitchClass, Octave)
> ro, mt, fo, fi, ms :: Octave -> Dur -> Music BluesPitch
> ro o d = note d (Ro, o)
> mt o d = note d (MT, o)
> fo o d = note d (Fo, o)
> fi o d = note d (Fi, o)
> ms o d = note d (MS, o)
> translate :: BluesPitch -> Pitch
> translate (Ro, o) = (C, o)  -- root = C
> translate (MT, o) = (Ef, o) -- Minor third = E flat
> translate (Fo, o) = (F, o)  -- fourth = F
> translate (Fi, o) = (G, o)  -- fifth = G
> translate (MS, o) = (Bf,o) -- minor seventh = B flat
> fromBlues :: Music BluesPitch -> Music Pitch
> fromBlues (Prim (Note d p)) = (Prim (Note d (translate p)))
> fromBlues (Prim (Rest d)) = Prim (Rest d)
> fromBlues (m1 :+: m2) = (fromBlues m1) :+: (fromBlues m2)
> fromBlues (m1 :=: m2) = (fromBlues m1) :=: (fromBlues m2)
> fromBlues (Modify c m) =  Modify c (fromBlues m)

Which should allow us to write simple melodies:

> bluesMel :: Music BluesPitch
> bluesMel = ro 4 qn :+: mt 4 qn :+: fo 4 qn :+: fi 4 qn :+: ms 4 qn
> bluesChord :: Music BluesPitch
> bluesChord = ro 4 qn :=: mt 4 qn :=: fi 4 qn
> bluesHarm  = instrument Harmonica bluesChord

And then just: `play (fromBlues bluesMel)`.

I'm not 100% sure if I needed `translate` (could have also been a local definition
for the base case of fromBlues), but I like how Music being polymorphic
made it easy to mostly define things in its terms. It's spooky, and satisfying,
that knowing that the types match means that it'll *work*.

2.4 Absolute Pitches
====================

There's a useful function that translates a pitch to an integer. Given a (relative)
pitch, it's defined as 12 times the octave plus the index of the pitch class:

absPitch :: Pitch -> AbsPitch
absPitch (pc, oct) = 12 * (oct + 1) + pcToInt pc

where pcToInt assigns numbers between -2 and 13 to Cff to Bss:

pcToInt :: PitchClass -> Int
pcToInt pc = case pc of
  Cff -> -2; Cf -> -1 ...

To transform an absolute pitch to a pitch is a bit more tricky, due to the
enharmonics. So Euterpea returns the sharp in those cases:

pitch :: AbsPitch -> Pitch
pitch ap =
  let (oct, n) = divMod ap 12 -- divMod x n returns a pair (q,r), where 1 is the integer quotient of x/n, and r is x%n
  in ([C, Cs, D, ...] !! n, oct - 1)

Where the `list !! n` returns the (n + 1)th element in `list`.

And with this, we can now define `trans`:

trans :: Int -> Pitch -> Pitch
trans i p = pitch (absPitch p + i)

Exercise 2.5:
-------------

Define a recursive function transM :: AbsPitch -> Music Pitch -> Music Pitch
that changes each note in a Music Pitch by transposit it by the AbsPitch interval.

> transM :: AbsPitch -> Music Pitch -> Music Pitch
> transM ap (Prim (Note d p)) = Prim (Note d (trans ap p))
> transM ap (Prim (Rest d)) = Prim (Rest d)
> transM ap (m1 :+: m2) = (transM ap m1) :+: (transM ap m2)
> transM ap (m1 :=: m2) = (transM ap m1) :=: (transM ap m2)
> transM ap (Modify c m)   = Modify c (transM ap m)

To prove that it works, let's take our beloved C chord, which transposed 7
should be G major:

> cMajChord = c 4 qn :=: e 4 qn :=: g 4 qn
> oneFive = cMajChord :+: (transM 7 cMajChord)

λ> cMajChord
Prim (Note (2 % 1) (C,4)) :=: (Prim (Note (2 % 1) (E,4)) :=: Prim (Note (2 % 1) (G,4)))
λ> (transM 7 cMajChord)
Prim (Note (2 % 1) (G,4)) :=: (Prim (Note (2 % 1) (B,4)) :=: Prim (Note (2 % 1) (D,5)))

Looking at the notes... it worked!


One interesting thing: Euterpea does this sort of similarly, but they
extract the repetition of matching all Music constructors:

https://github.com/Euterpea/Euterpea2/blob/bc89ee9a47fb06ef5908208609064789fab994a3/Euterpea/Music.lhs#L449-L453

+

https://github.com/Euterpea/Euterpea2/blob/bc89ee9a47fb06ef5908208609064789fab994a3/Euterpea/Music.lhs#L478-L479

In particular, given mMap which basically abstracts away the tedium
matching Music, a transposition function similar to the above becomes:

shiftPitches :: AbsPitch -> Music Pitch -> Music Pitch
shiftPitches k = mMap (trans k)
