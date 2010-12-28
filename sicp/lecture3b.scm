(define (deriv exp var)
    ;we define the derivatives of expressions in respect to variables
    ;as DISPATCHES based on TYPE (kinda like OOP abstraction in ruby)
    ;or this: http://okmij.org/ftp/Scheme/oop-in-fp.txt
    (cond ((constant? exp var) 0)
          ((same-var? exp var) 1)
          ((sum? exp) 
            (make-sum (deriv (a1 exp) var)
                      (deriv (a2 exp) var)))
          ((product? exp)
            (make-sum 
                (make-product (m1 exp)
                              (deriv (m2 exp) var))
                (make-product (deriv (m1 exp) var)
                              (m2 exp))))
           ;more rules for derivatives...
           ))

(define (constant? exp var)
    (and (atom? exp) ;an atom hasn't car or cdr: it can't be broken in sub-parts
         (not (eq? exp var))))

(define (same-var? exp var)
    (and (atom? exp)
         (eq? exp var)))

(define (sum? exp)
    (and (not (atom? exp))
         (eq (car exp) '+))) ;=> quotation to distinguish between the operation or operator

;sussman exemplifies the quotation to enter "data mode" by telling:
; 'say your name', which is ambiguous and could be 'say your name' or 'say "your name"'

(define (make-sum a1 a2)
    (list '+ a1 a2))

(define a1 cadr)  ; (a1 '(+ 3 5)) => 3
(define a2 caddr) ; (a2 '(+ 3 5)) => 5

(define (product? exp)
    (and (not (atom? exp))
         (eq? (car exp) '*)))

(define (make-product m1 m2)
    (list '* m1 m2))

(define m1 cadr)  ; (m1 '(+ 3 5)) => 3
(define m2 caddr) ; (m2 '(+ 3 5)) => 5

;here's a refinement of the above:
;if the operation can be made, make it
(define (make-sum a1 a2)
    (cond ((and (number? a1) (number? a2)) (+ a1 a2))
          ((and (number? a1) (= a1 0) a2))
          ((and (number? a2) (= a2 0) a1))
          (else (list '+ a1 a2))))
