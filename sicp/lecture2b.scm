(define (sum-rat x y)
    (make-rat 
        (+ (* (numer x) (denom y))
           (* (numer y) (denom x)))
        (* (denom x) (denom y)))) 

(define (*rat x y)
    (make-rat
        (* (numer x) (numer y))
        (* (denom x) (denom y))))

;make rational numbers simple from build time
(define (make-rat x y)
    (let((g (gcd x y)))
      (cons (/ x g)
            (/ y g))))

(define (numer x)
    (car x))

(define (denom x)
    (cdr x))

(define make-point 
    (lambda (x y)
        (cons x y)))

(define (xcor v)
    (car v))

(define (ycor v)
    (cdr v))

(define make-seg 
    (lambda (s e)
        (cons s e)))
 
(define (seg-start s)
    (car s))

(define (seg-end s)
    (cdr s))

(define (midpoint s)
    (let ((a (seg-start s))
          (b (seg-end   s)))
         (make-point 
            (average (xcor a) (xcor b))
            (average (ycor a) (ycor b)))))

;TODO: write the length function
;redefining cons, car and cdr (pronounced could-r)

(define (cons a b)
    ;return a procedure that takes an index and returns the element in that index
    (lambda (pick)
        (cond ((= pick 1) a)
              ((= pick 2) b))))
;hence, car of a cons is just the cons-lambda called with one
(define (car x) (x 1))
;and cdr, the cons-lambda called with two!
(define (cdr x) (x 2))

;WOW: this guy illustrated the layered abstraction of data by demonstrating that cons
;cad and cdr can also be seen as abstracted methods
