#lang racket

;;; A full working version of this can be found in:
;;; "/Applications/Racket v7.1/share/pkgs/realm/chapter5/ufo-source.rkt"
;;; where `/Applications/Racket v7.1/` is the base dir for the installation

(require 2htdp/universe 2htdp/image)

(define WIDTH  100)
(define HEIGHT 200)
;;(define IMAGE-of-UFO ) -- only dr Racket knows about images in source

(define (add-3-to-state current-state)
  (+ current-state 3))

(define (draw-ufo current-state)
  (place-image
   IMAGE-of-UFO (/ WIDTH 2) current-state
   (empty-scene WIDTH HEIGHT)))

(define (state-is-300 current-state)
  (>= current-state 300))


;; big-bang is a macro that takes an initial world and a bunch of
;; handlers and closes over them.
;; Racket macros are definitely less... palatable than CL/Clojure macros:
;; https://github.com/racket/htdp/blob/master/htdp-lib/2htdp/universe.rkt#L265-L288
;; Docs at: https://docs.racket-lang.org/teachpack/2htdpuniverse.html#%28form._world._%28%28lib._2htdp%2Funiverse..rkt%29._big-bang%29%29
;; One neat thing to note however, is how big-bang sorta behaves like a class/closure/object, handing the state to its handlers whenever they're called; could take a page out of that book for the GUI abstraction!

(define (main)
  (big-bang 0
            (on-tick add-3-to-state)
            (to-draw draw-ufo)
            (stop-when state-is-300)))
