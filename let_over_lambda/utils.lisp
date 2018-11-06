;; Defined in Chapter 1
;; utility functions from Graham's On Lisp


(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                              (subseq source 0 n)
                              acc))
                   (nreverse
                    (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc))))))
    (rec x nil)))

;; Some illustrative output:

;; CL-USER> (mkstr 'b 'a)
;; "BA"
;; CL-USER> (symb 'b)
;; B
;; CL-USER>
;; CL-USER> (group '(a b c d) 2)
;; ((A B) (C D))
;; CL-USER>
;; (flatten '(1 (2 3) ((4 5) 6)))
;; (1 2 3 4 5 6)
;; CL-USER> 

;; also, C-c C-d d is very useful for looking up the documentation of certain fns!
