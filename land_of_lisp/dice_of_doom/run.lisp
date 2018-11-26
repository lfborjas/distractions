;; Loads all the files in order of definition,
;; since there's some stuff that was incrementally
;; improved to illustrate lazy evaluation, memoization,
;; chance nodes, and AI improvements like alpha-beta
;; pruning, and chance nodes for overall better gameplay
(load "dice_of_doom_v1.lisp")

(load "lazy.lisp")
(load "dice_of_doom_v2.lisp")

(load "webserver.lisp")
(load "svg.lisp")
(load "dice_of_doom_v3.lisp")

(load "dice_of_doom_v4.lisp")

;; you can redefine parameters here, for example, if you want more
;; humans to play, you can say (leaves the AI as the second player):
; (defparameter *human-players* '(0 2 3))
;; check out the defparameters in the source to see what you can tinker with
;; not all of them are wise to mess with:
;; dice_of_doom_v2.lisp:(defparameter *board-size* 5)
;; dice_of_doom_v2.lisp:(defparameter *board-hexnum* (* *board-size* *board-size*))
;; dice_of_doom_v3.lisp:(defparameter *board-width*  900)
;; dice_of_doom_v3.lisp:(defparameter *board-height* 500)
;; dice_of_doom_v3.lisp:(defparameter *board-scale*  64)
;; dice_of_doom_v3.lisp:(defparameter *top-offset*   3)
;; dice_of_doom_v3.lisp:(defparameter *dice-scale*   40)
;; dice_of_doom_v3.lisp:(defparameter *dot-size*     0.05)
;; dice_of_doom_v3.lisp:(defparameter *die-colors*   '((255 63 63)
;; dice_of_doom_v3.lisp:(defparameter *cur-game-tree* nil)
;; dice_of_doom_v3.lisp:(defparameter *from-tile* nil)
;; dice_of_doom_v3.lisp:(defparameter *human-players* '(0 2))
;; dice_of_doom_v3.lisp:(defparameter *cur-human-player* 0) ; we always start with the first human player
;; dice_of_doom_v4.lisp:(defparameter *num-players* 4)
;; dice_of_doom_v4.lisp:(defparameter *max-dice* 5)
;; dice_of_doom_v4.lisp:(defparameter *ai-level* 2) ;; dumb it down a bit since there'll be more heuristics
;; (defparameter *die-colors*   '((255 63 63) ; red
;;                                (63 63 255) ; blue
;;                                (63 255 63) ; green
;;                                (255 63 255) ;purple
;;                                ))


(serve #'dod-request-handler)
