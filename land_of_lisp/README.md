# Land of Lisp

I began reading a PDF copy of this book in 2011 and worked through some
examples, a Lisp neophyte, fascinated and frustrated by all the parens (I was
using a barebones installation of vim, no REPL integration, nothing of the
life-changing benevolence of Emacs's Paredit, rainbow delimiters and SLIME).

Picked it back up in 2018 (now a print copy) as a study into the cross-pollination in the Lisp
world while ramping up my use of Clojure. There's many good things in this
little tome, though I speed-read through the first 13 chapters to get to the
good stuff: the scrappy, crazy little web server, svg renderer and game engine
for Dice of Doom (which I've also implemented while [following Realm of
Racket](https://github.com/lfborjas/distractions/tree/master/realm_of_racket). 

## Setup

Even though I use SBCL for my study of Let over Lambda, Land of Lisp encourages
the use of CLISP for its implementation of sockets. Considering that what I
wanted to see was the web server, I acquiesced. If you're on OS X and use
homebrew, there's a formula:

	brew install clisp

Which should put the binary in your `bin` folder. If you use Emacs + SLIME, 
configuration should be easy:

	(setq inferior-lisp-program "/usr/local/bin/clisp")
	(setq slime-contribs '(slime-fancy))
	(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

## Running

I run my stuff from the REPL from within Emacs. `C-c C-k` for entire files, `C-x
C-e` for individual sexps.

## Games

### Dice of Doom, v1-2: text-based

An example of DoD, v1, Human vs. Human; using Emacs + SLIME:

![image](https://user-images.githubusercontent.com/82133/48965920-1c5fb500-ef95-11e8-99fd-84651bbd6909.png)

### Dice of Doom, v3: web based

Playing against a computer using the makeshift webserver from Chapter 13:

<img width="1037" alt="sample DoD game" src="https://user-images.githubusercontent.com/82133/48992744-88622a80-f107-11e8-9fba-0ce9d685fcf1.png">

### Dice of Doom, v4 (final): web based with chance nodes and 4 players

![image](https://user-images.githubusercontent.com/82133/48995653-d6caf580-f116-11e8-871a-110ff54666ad.png)

For this, I wrote `run.lisp` and compiled it for ultimate optimization (with `clisp -c run.lisp`). One can run the game from the command line, with `clisp run.fas`:

```sh
λ ~/Play/distractions/land_of_lisp/dice_of_doom/ master* clisp -c run.lisp
  i i i i i i i       ooooo    o        ooooooo   ooooo   ooooo
  I I I I I I I      8     8   8           8     8     o  8    8
  I  \ `+' /  I      8         8           8     8        8    8
   \  `-+-'  /       8         8           8      ooooo   8oooo
    `-__|__-'        8         8           8           8  8
        |            8     o   8           8     o     8  8
  ------+------       ooooo    8oooooo  ooo8ooo   ooooo   8

Welcome to GNU CLISP 2.49 (2010-07-07) <http://clisp.cons.org/>

Copyright (c) Bruno Haible, Michael Stoll 1992, 1993
Copyright (c) Bruno Haible, Marcus Daniels 1994-1997
Copyright (c) Bruno Haible, Pierpaolo Bernardi, Sam Steingold 1998
Copyright (c) Bruno Haible, Sam Steingold 1999-2000
Copyright (c) Sam Steingold, Bruno Haible 2001-2010

Type :h and hit Enter for context help.

;; Compiling file /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/run.lisp ...
;; Wrote file /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/run.fas
The following functions were used but not defined:
 SERVE DOD-REQUEST-HANDLER
0 errors, 0 warnings
Bye.
```

Due to the redefinitions as I went through the book, some warnings are expected:

```sh
Bye.
λ ~/Play/distractions/land_of_lisp/dice_of_doom/ master* clisp run.fas 
WARNING: DEFUN/DEFMACRO: redefining function ADD-PASSING-MOVE in /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v2.lisp, was defined in
         /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v1.lisp
WARNING: DEFUN/DEFMACRO: redefining function ATTACKING-MOVES in /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v2.lisp, was defined in
         /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v1.lisp
WARNING: DEFUN/DEFMACRO: redefining function HANDLE-HUMAN in /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v2.lisp, was defined in
         /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v1.lisp
WARNING: DEFUN/DEFMACRO: redefining function PLAY-VS-HUMAN in /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v2.lisp, was defined in
         /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v1.lisp
WARNING: DEFUN/DEFMACRO: redefining function HANDLE-COMPUTER in /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v2.lisp, was defined in
         /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v1.lisp
WARNING: DEFUN/DEFMACRO: redefining function PLAY-VS-COMPUTER in /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v2.lisp, was defined in
         /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v1.lisp
WARNING: DEFUN/DEFMACRO: redefining function GET-RATINGS in /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v2.lisp, was defined in
         /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v1.lisp
WARNING: DEFUN/DEFMACRO: redefining function RATE-POSITION in /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v2.lisp, was defined in
         /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v1.lisp
WARNING: DEFUN/DEFMACRO: redefining function ATTACKING-MOVES in /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v4.lisp, was defined in
         /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v2.lisp
WARNING: DEFUN/DEFMACRO: redefining function WEB-HANDLE-HUMAN in /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v4.lisp, was defined in
         /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v3.lisp
WARNING: DEFUN/DEFMACRO: redefining function HANDLE-COMPUTER in /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v4.lisp, was defined in
         /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v2.lisp
WARNING: DEFUN/DEFMACRO: redefining function GET-RATINGS in /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v4.lisp, was defined in
         /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v2.lisp
WARNING: DEFUN/DEFMACRO: redefining function LIMIT-TREE-DEPTH in /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v4.lisp, was defined in
         /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v2.lisp
WARNING: DEFUN/DEFMACRO: redefining function ADD-NEW-DICE in /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v4.lisp, was defined in
         /Users/lfborjas/Play/distractions/land_of_lisp/dice_of_doom/dice_of_doom_v1.lisp
```

It'll just sit there until killed with Ctrl-C

The computer plays with its 3 players and is... very smart:

![image](https://user-images.githubusercontent.com/82133/48995771-522ca700-f117-11e8-81e8-5ffa2f135129.png)

