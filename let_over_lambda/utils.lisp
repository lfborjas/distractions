;;; functions and macros referenced throughout the book:

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
                   ;; The SBCL implementation of comma made quoted expressions flatten weird, hence the addition of this here:
                   ;; https://github.com/thephoeron/let-over-lambda/blob/f321d9524b749730d3e40ed214ec5026750d40f9/let-over-lambda.lisp#L65
                   ;; and some nerds talking about it here:
                   ;; https://www.reddit.com/r/learnlisp/comments/6bdhrq/sbcl_doesnt_flatten_quasiquote_commas_the_way/
                   ((typep x 'sb-impl::comma) (rec (sb-impl::comma-expr x) acc))
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

;;; Defined in Chapter 3

;; see if a symbol is a "g-bang" symbol
;; standardizing how we "name" our gensyms: any symbol that starts with g! and is followed by at least another character.
(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "G!"
                :start1 0
                :end1 2)))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(subseq
                               (symbol-name s)
                               2))))
              syms)
         ,@body))))

(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "O!"
                :start1 0
                :end1   2)))

(defun o!-symbol-p-to-g!-symbol (s)
  (symb "G!"
        (subseq (symbol-name s) 2)))

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-p-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))
