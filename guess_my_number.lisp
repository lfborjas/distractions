;First code example in the land of lisp book
;december 2, 2010

;implements binary search to guess a number...

;we could use defvar to define CONSTANTS instead of GLOBAL vars
;the ear-muffs are optional, but are a style convention for global stuff
(defparameter *small* 1)

(defparameter *big* 100)

;the ash function performs an arithmetic shift in the binary representation of the number
;thus, we start with 101, which reads:
; 1100101
;if we tell ash to move it by -1, it'll shift the bits one place to the RIGHT, thus:
; 110010
;which is 50, effectively halving the guesses!
(defun guess-my-number () 
    (ash (+ *small* *big*) -1))

(defun smaller ()
    (setf *big* (1- (guess-my-number) ))
    (guess-my-number))

(defun bigger ()
    (setf *small* (1+ (guess-my-number) ))
    (guess-my-number))

(defun start-over ()
    (defparameter *small* 1)
    (defparameter *big*   100)
    (guess-my-number))

; let is for local vars: (let (declaration*) body)
; and flet, for a local function (flet ((f (args) body)*) body)
; note that function declarations in flet don't know about each other, so ,
; if local functions need be defined in terms of others, we use `labels`
