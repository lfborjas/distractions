;; Following: https://download.racket-lang.org/releases/7.1/doc/quick/index.html
;; With some emacs love from: https://docs.racket-lang.org/guide/Emacs.html (I installed racket-mode)

#lang slideshow

(define (square n)
  (filled-rectangle n n))

(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))

(checker (colorize (square 10) "red")
         (colorize (square 10) "black"))
