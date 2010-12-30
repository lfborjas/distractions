;pattern matching to build DSLs
;this is how it works:

;pattern -----> skeleton
; |                |
; | (matching)     | (instantiation)
; v                v
;source exp --> target exp
    
(define deriv-rules
    '(
        ;derivative of a constant c with respect to a variable v is zero
        ( (dd (?c c) (? v))     0)
        ;derivative of a var v in respect to itself is one
        ( (dd (?v v) (? v))     1)
        ;derivative of a var u with respect to another, 0
        ( (dd (?v u) (? v))     0)  

        ;derivative of the sum of two numbers in respect to a var v
        ( (dd (+ (? x1) (? x2)) (? v))
           
                                        (+ (dd (: x1) (: v))
                                           (dd (: x2) (: v))))
        ;... more rules (product and exponentiation)
        )
)

;a program that simplify exps given some rules:
;>> (dsimp '(dd (+ x y) x)
;=> (+ 1 0) ;because d(x+y)/dx = dx/dx + dy/dx = 1 + 0 
;(define dsimp 
;    (simplifier deriv-rules)

;the matcher:
; the dictionary is the accumulative results of walking down the
; expression and pattern trees simultaneously:
(define (match pat exp dict)
    (cond ((eq? dict 'failed) 'failed) ; the failures are propagated down the stack
          ((atom? pat)
            (if (atom? exp)
                (if (eq? pat exp)
                    dict     ;all's well, propagate
                    'failed) ;being atoms, they should be the same thing
                'failed ; the exp should be atomic too
            ))

          ;now, the three kinds of pattern variables:
          ((arbitrary-constant? pat)
            (if (constant? exp)
                (extend-dict pat exp dict) ;all's well, update the dict
                'failed)) ; the expression should be a constant
          ((arbitrary-variable? pat)
            (if (variable? exp)
                (extend-dict pat exp dict)
                'failed))
          ((arbitrary-expression? pat) (extend-dict pat exp dict))

          ((atom? exp) 'failed) ; I couldn't reach this point with an atomic expression: the pattern should
                                ; be atomic too, but that's BEFORE this condition!
          ;here comes the recursion: keep walking down the three
          (else 
            (match (cdr pat) ;remember, we are walking down the TWO
                   (cdr exp) ; trees simultaneously
                   (match (car pat)
                          (car exp)
                          dict)))))

;the instantiator:
(define (instantiate skeleton dict)
    (define (loop s)
        (cond ((atom? s) s)
              ((skeleton-evaluation? s)
                (evaluate (eval-exp s) dict)) ;look it up in the dict
              (else (cons (loop (car s))
                          (loop (cdr s))))))
    (loop skeleton))

(define (evaluate form dict)
    (if (atom? form) (lookup form dict)
        (apply 
            (eval (lookup (car form) dict)
            user-initial-environment)
            (mapcar (lambda (v) 
                        (lookup v dict))
                    (cdr form)))))

;something to take rules and simplify them
(define (simplifier the-rules)
    (define (simplify-exp exp)
       (try-rules (if (compound? exp)
                      (simplify-parts exp); OR, i could get rid of simplify-parts and just write (map simplify-exp exp)
                      exp)))
    (define (simplify-parts exp)
        (if (null? exp)
            '()
            (cons (simplify-exp (car exp))
                  (simplify-parts (cdr exp)))))
    (define (try-rules exp)
        (define (scan rules)
           (if (null? rules)
               exp
               (let ((dict ;assign to 'dict' the dictionary built by a matcher
                        (match (pattern (car rules))
                               exp
                               (empty-dictionary))))
                  (if (eq? dict 'failed)
                      (scan (cdr rules))
                      (simplify-exp 
                        (instantiate
                            (skeleton (car rules))
                            dict))))))
        (scan the-rules))
    simplify-exp)

(define (empty-dictionary) '())

(define (extend-dict pat dat dict)
    (let ((name (variable-name pat)))
       (let ((v (assq name dict)))
         (cond ((null? v)
                    (cons (list name dat) dict)) ;if it isn't, put it
               ((eq? (cadr v) dat) dict); if it IS, it BETTER BE the same
               (else 'failed)))))

