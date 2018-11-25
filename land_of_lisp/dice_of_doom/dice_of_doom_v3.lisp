;;; Chapter 19: a Web UI for Dice of Doom

;;; Requires code from previous chapters:
;;; (load "dice_of_doom_v2.lisp")
;;; (load "webserver.lisp")
;;; (load "svg.lisp")

;;; CONSTANTS

(defparameter *board-width*  900)
(defparameter *board-height* 500)
(defparameter *board-scale*  64)
(defparameter *top-offset*   3)
(defparameter *dice-scale*   40)
(defparameter *dot-size*     0.05)
