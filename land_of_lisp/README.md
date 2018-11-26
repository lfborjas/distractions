# Land of Lisp

I began reading a PDF copy of this book in 2011 and worked through some
examples, a Lisp neophyte, fascinated and frustrated by all the parens (I was
using a barebones installation of vim, no REPL integration, nothing of the
life-changing benevolence of Emacs's Paredit, rainbow delimiters and SLIME).

Picked it back up in 2018 as a study into the cross-pollination in the Lisp
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

