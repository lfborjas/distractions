(define (-c z1 z2)
  (make-rectangular 
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))

(define (+c z1 z2)
  (make-rectangular 
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

(define (*c z1 z2)
  (make-polar 
   (* (magnitude z1) (magnitude z2))
   (+ (angle z1) (angle z2))))

(define (/c z1 z2)
  (make-polar 
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))

;now, one representation:
(define (make-rectangular x y)
  (cons x y))

(define real-part (lambda(z) (car z)))

(define imag-part (lambda(z) (cdr z)))

(define (make-polar r a)
  (cons (* r (cos a)) (* r (sin a))))

(define (magnitude z)
  (sqrt (+ (square (car z))
           (square (cdr z)))))

(define (angle z)
  (atan (cdr z) (car z)))

;however, one could represent them like this:
(define (make-polar r a)
  (cons  r a))

(define (magnitude z)
  (car z))

(define (angle z)
  (cdr z))

(define (make-rectangular x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define real-part (lambda (z)
  (* (car z) (cos (cdr z)))))

(define (imag-part z)
  (* (car z) (sin (cdr z))))

;so, which is better? Maybe with lots of additions, the first; and with products, the second...

;lo and behold: typed data:
(define (attach-type type contents)
  (cons type contents))

(define type (lambda (datum)
               (car datum)))
(define (contents datum)
  (cdr datum))

(define (rectangular? z)
  (eq? (type z) 'rectangular))

(define (polar? z)
  (eq? (type z) 'polar))

;so, now, the two implementations can co-exist:
(define (make-rectangular x y)
  (attach-type 'rectangular (cons x y)))

(define real-part-rectangular (lambda(z) (car z)))

(define imag-part-rectangular (lambda(z) (cdr z)))

;and so on and so forth with the rest of methods: append the type at the end:
(define (magnitude-rectangular z)
  (sqrt (+ (square (car z))
           (square (cdr z)))))

(define (angle-rectangular z)
  (atan (cdr z) (car z)))

;and the same with polar
(define (make-polar r a)
  (attach-type 'polar (cons  r a)))

(define real-part-polar (lambda (z)
  (* (car z) (cos (cdr z)))))

(define (imag-part-polar z)
  (* (car z) (sin (cdr z))))

(define (magnitude-polar z)
  (car z))

(define (angle-polar z)
  (cdr z))

;and here come the GENERIC selectors: with DISPATCH ON TYPE
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular 
          (contents z)))
        ((polar? z)
         (real-part-polar
          (contents z)))))

;and the same for the other ones, but there must be a better way...

;criticisms:
;1. had to rename the procedures
;2. what happens if there is a new implementation? Gotta change the dispatch procedures!
;but, if some sort of table exists wherein we can:
;(PUT key1 key2 value)
;and
;(GET key1 key2)
;so, instead of having the hard-coded dispatch procedures, we'd do something like:
;called DATA-DIRECTED PROGRAMMING
(put 'rectangular 'real-part real-part-rectangular)
;and so on with every implementation

(define (operate operator object)
  (let ((proc (get (type object) operator)))
    (if (not (null? proc))
        (proc (contents object))
        (error "undefined operator"))))

;and the selectors become:
(define (real-part obj)
  (operate 'real-part obj))


;now the more general operate: for disjunct types
(define (operate-2 op arg1 arg2)
  (if (eq? (type arg1) (type arg2))
      (let ((proc (get (type arg1) op)))
        (if (not (null? proc))
            (proc (contents arg1)
                  (contents arg2))
            (error "op undefined on type")))
      (error "args not the same type")))
        
           

;but this one requires mantaining the operations table (everyone has to "put" their stuff there)
;I'd like to automate it even more! (by looking it up in the namespace, perhaps?)
