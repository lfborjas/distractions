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
