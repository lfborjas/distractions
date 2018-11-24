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