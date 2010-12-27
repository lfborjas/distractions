;lecture 3a
;(scale-list 10 '(1 2 3)) -> (10 20 30)

;the first solution:
;it shows that there is a general procedure for doing *something* to *every* element in the list
(define (scale-list s l)
    (if (null? l)
        nil
        (cons (* (car l) s)
              (scale-list s (cdr l)))))

;the general procedure abstracted from the above pattern:
;when you find a pattern, turn it into a higher order procedure!
;this is how you derive MAP!
;remember that map *returns a _new_ list*
(define (map p l)
    (if (null? l)
        nil
        (cons (p (car l))
              (map p (cdr l)))))
;how would you write map recursively?
;this is my solution to it, using map:
(define (scale-list n l) 
    (map 
        (lambda (x) (* n x)) 
        l))

;for-each, in turn, is like map but doesn't return anything
(define (for-each proc list)
    (cond ((null? list) "done")
          (else (proc (car list))
                (for-each proc
                          (cdr list)))))
        

