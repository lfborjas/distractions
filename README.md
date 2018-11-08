# Distractions

Working through examples found in various books or online resources.

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




# Distractions thus far

## Land of Lisp

A very entertaining book that I began reading in 2011 and picked back up in 2018: http://landoflisp.com

## Let Over Lambda

Very opinionated and deeply interesting exploration of Common Lisp macros, interested in macro design for my Clojure explorations. A good chunk of the book is available online, at: https://letoverlambda.com

