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

(serve #'dod-request-handler)
