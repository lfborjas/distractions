;introducing set! for state/side effects

;let/define are part of the SUBSTITUION MODEL of functional programming
;but set! is part of the ENVIRONMENT model of imperative programming

;concepts:

;1. bound and free variables

;Bound variable: one which can be replaced by another symbol and doesn't
;change the meaning of the expression:

(lambda (x  y) 
    (+ x y))

;is equivalent to
(lambda (x z)
    (+ x z))

;X and Y are bound, because changing one or the other by another symbol does
;NOT alter  the meaning of the expression

;free variable: one that DOES alter the expression if replaced:

(define y 3)
(define z 4)

(lambda (x)
    (+ x y))

;is not equivalent to
(lambda (x)
    (+ x z))

;because y and z are free, and might introduce side effects.

;even more, in this procedure:

(lambda (x y) 
    (* x y))

;the ASTERISK is a free variable, because if we where to change that
;symbol with a PLUS symbol, the meaning of the expression would change!

;2. SCOPE
; wherever a variable is defined as part of the argument list of a lambda
; expression ('cause, ultimately, only lambda binds stuff)

;remember that:
(let 
    ((x 1)
     (y 2))
    (+ x y))

;is actually the same as

(
    (lambda (x y)
            (+ x y))
     1
     2)

(define make-counter 
    (lambda (n)
        (lambda ()
            (set! n (+ 1 n))
            n)))
