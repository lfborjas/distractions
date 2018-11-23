# Distractions

Working through examples found in various books or online resources. Mostly functional languages, with a heavy bias towards lisps. For something slightly close to reality, see my [clojure-playground](https://github.com/lfborjas/clojure-playground). For things actually close to reality, I'm afraid all my battle-tested production code that makes money is in private repos!

# Up and running (a note to self)

## With Racket

Download Racket from the official website: https://racket-lang.org

It'll prompt you to drag the Racket folder to the applications folder. Though in my case, it was in some weird non-standard `Applications` (in `/Volumes/Macintosh\ SD/Applications`?). DrRacket will be openable as an application, but you'll have to symlink the racket binary yourself if you want it. I did the following:

	ln -s /Volumes/Macintosh\ HD/Applications/Racket\ v7.1/bin/racket /usr/local/bin/racket

You'll notice a few other apps (with corresponding bins): 

	ls /Volumes/Macintosh\ HD/Applications/Racket\ v7.1/
	DrRacket.app/             Racket Documentation.app/ collects/                 include/                  share/                    
	PLT Games.app/            Slideshow.app/            doc/                      lib/                      
	README.txt                bin/                      etc/                      man

For example, `PLT Games` comes with a cute old-skool looking set of classic games like minesweeper!


## With Common Lisp

I found this to be the best resource to get a working Common Lisp installed: https://exercism.io/tracks/common-lisp/installation

For posterity, the relevant steps/sections:

### Installing SBCL on Mac OSX

	brew install sbcl

### Installing Quicklisp:

Official site: https://www.quicklisp.org/beta/#installation

Download quicklisp:

	curl -O http://beta.quicklisp.org/quicklisp.lisp


Launch the SBCL REPL in the same dir and then:

	* (load "quicklisp.lisp")         ;; this will load the downloaded lisp file
	* (quicklisp-quickstart:install)  ;; this will install quicklisp
	* (ql:add-to-init-file)           ;; this will add quicklisp setup to your init file (recommended)

That last step is important: you'll have quicklisp loaded every time you launch a new SBCL repl now.

Other okay introductory resources for installation:

* https://lisp-lang.org/learn/getting-started/
* http://stevelosh.com/blog/2018/08/a-road-to-common-lisp/

### Installing SLIME

I use Emacs for my lisping (and anything really). SLIME is the archetypal interactive mode, and I got it going with:

    M-x package-install RET slime RET
    
From within Emacs itself. And added the following to a file in my emacs config (put it in `emacs.d/customizations/setup-common-lisp.el`):

    (setq inferior-lisp-program "/usr/local/bin/sbcl")
    (setq slime-contribs '(slime-fancy))
    (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
    
    

From: https://github.com/slime/slime#quick-setup-instructions

More about Slime contrib: https://www.common-lisp.net/project/slime/doc/html/Loading-Contribs.html

With the contribs in place, one can just launch slime with `M-x slime` and the better REPL (i.e. not the default SBCL `*` prompt with no autocompletion) should show up.

My current Emacs setup for  looks like this:

![image](https://user-images.githubusercontent.com/82133/48101662-d2857980-e1f5-11e8-97a7-dc8e3162e0e4.png)


# Distractions thus far

## Land of Lisp

A very entertaining book that I began reading in 2011 and picked back up in 2018: http://landoflisp.com. Revels in the oddity of Common Lisp and sets out to write a few text-based (and one browser-based) games from scratch, exploring most of the capabilities of the language.

## Let Over Lambda

Very opinionated and deeply interesting exploration of Common Lisp macros, interested in macro design for my Clojure explorations. A good chunk of the book is available online, at: https://letoverlambda.com

## Realm of Racket

A gentle introduction to Racket, using graphics libraries bundled with the language and useful for following another introductory book, [How to Design Programs, 2nd Edition](https://htdp.org/2018-01-06/) to build a few games to illustrate concepts such as recursion, structs (mutable and immutable), higher order functions and the `for` family of macros, lazy evaluation, decision tree exploration and pruning and even networking! Not as ambitious or intense as Land of Lisp, but definitely in the same spirit and conducive to picking up some idiomatic Racket practices laid down by one of the creators of the language.

## Haskell School of Music

Finally exploring the other side of the functional house lured by my love of music. This book explores the Euterpea haskell library as well as the language itself. Came out recently (August 2018): http://euterpea.com/haskell-school-of-music/ (the full text of an earlier, incomplete (due to the untimely death of the original author), version is available at: http://haskell.cs.yale.edu/wp-content/uploads/2015/03/HSoM.pdf)

