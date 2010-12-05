;implicit primitives:
(define (square a) (* a a))

(define (1+ a) (+ 1 a))

(define (average x y) 
        (/ (+ x y) 2))

;discovering a pattern in the sigma notation...
(define (sum-int-lame a b)
    (if (> a b)
        0
        (+ a (sum-int (+ a 1) b))))


(define (sum-sq-lame a b)
    (if (> a b)
        0
        (+ (square a) (sum-sq (+ a 1) b))))

;generalize with higher order functions
(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a) (sum term 
                         (next a)
                         next
                         b))))

;rewriting the originals:


(define (sum-int a b)
    (define (identity a) a)
    (sum identity a 1+ b))
    
(define (sum-sq a b)
    (sum square a 1+ b))

;using lambdas!
(define (pi-sum a b)
    (sum (lambda (i) (/ 1 (* i (+ i 2))))
         a
         (lambda (i) (+ i 4))
         b))


;iterative implementation of sum
;only call this one above, instead of changing each!
(define (sum-iter term a next b)
        (define (iter j ans)
            (if (> j b)
                ans
                (iter (next j)
                      (+ (term j) ans))))
         (iter a 0))

;SECOND PART

(define (fixed-point f start)
    (define tolerance 0.00001)
    (define (close-enough? u v)
        (< (abs (- u v)) tolerance))
    (define (iter old new)
        (if (close-enough? old new)
            new
            (iter new (f new))))
    (iter start (f start)))

(define (sqrt x)
    (fixed-point 
        (lambda (y) (average (/ x y) y))
        1))

;average-damping the oscillation between y and x/y
(define (sqrt x)
    (fixed-point
        (average-damp (lambda (y) (/ x y)))
        1))

;this procedure takes a procedure and produces *another* procedure which average-damps the original
(define average-damp 
    (lambda (f)
           (lambda (x) (average (f x) x))))

;THIRD PART

;newton's method for finding the zeroes of a function

(define (sqrt y)
    (newton (lambda (y) (- x (square y)))
    1))

(define (newton f guess)
    (define df (deriv f))
    (fixed-point 
        (lambda (x) (- x (/ (f x) (df x))))
        guess))

(define deriv 
    (lambda (f) 
        (lambda (x)
                (/ (- (f (+ x dx))
                      (f x))
                    dx))))
(define dx .000001)



