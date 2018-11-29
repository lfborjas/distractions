# Haskell School of Music

## Setup

Following the instructions in [the Euterpea
site](http://www.euterpea.com/download-and-installation/):

* Install [Haskell Platform](https://www.haskell.org/platform/) (v 8.4.3 as of the time of writing)
* Try to install Euterpea, but failed:

```
```

Had to edit this file `sudo vim
/Library/Frameworks/GHC.framework/Versions/8.4.3-x86_64/usr/lib/ghc-8.4.3/settings`
as recommended by [this stack overflow
answer](https://stackoverflow.com/questions/50386787/cabal-install-gcc-failed-in-phase-c-compiler/50419101#50419101)
as linked by [this github issue for haskell
platform](https://github.com/haskell/haskell-platform/issues/304).

The edit was simply changing this line to "NO":

```
, ("C compiler supports -no-pie","YES")
```

* Install the HSoM library, too, with `cabal install HSoM`.
* Set up MIDI: http://www.euterpea.com/euterpea/setting-up-midi/ (was as simple
  as downloading [SimpleSynth](http://notahat.com/simplesynth/).

After the above is done, and SimpleSynth is open, this should work (i.e. play a
sound):

```
λ ~/Play/distractions/haskell_school_of_music/ master* ghci
GHCi, version 8.4.3: http://www.haskell.org/ghc/  :? for help
Prelude> import Euterpea
Prelude Euterpea> play $ d 4 qn
Prelude Euterpea> 
```

## Emacs setup

Using [haskell-mode](http://haskell.github.io/haskell-mode/manual/latest/) as [recommended by the official wiki](https://wiki.haskell.org/Emacs), needed to do some setup (put this in a `setup-haskell.el` in my `~/.emacs.d`):

```elisp
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
```

And then running `C-c C-l` to compile a file (which opens a REPL if not already running), I was able to get a REPL going:

