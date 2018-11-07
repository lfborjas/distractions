;; Re-reading Let Over Lambda starting at chapter 3, this time with a proper Emacs + SLIME config going on to interact with the code faster.

(defmacro unit-of-time (value unit)
  `(* ,value
    ,(case unit
       ((s) 1)
       ((m) 60)
       ((h) 3600)
       ((d) 86400)
       ((ms) 1/1000)
       ((us) 1/1000000))))

;; one interesting note: in the book they typeset the backquote to look like a smart single quote, but it's just the backtick!


;; CL already has an NLET macro, got a warning here
(defmacro nlet. (n letargs &rest body)
  `(labels ((,n ,(mapcar #'car letargs)
              ,@body))
     (,n ,@(mapcar #'cadr letargs))))

(defun nlet-fact (n)
  (nlet fact ((n n))
        (if (zerop n) 1
            (* n (fact (- n 1))))))

;; nif-buggy violates referential transparency because it unwittingly introduces a binding, obscure-name, that may override a variable in the caller's environment

(defmacro nif-buggy (expr pos zero neg)
  `(let ((obscure-name ,expr))
     (cond ((plusp obscure-name) ,pos)
           ((zerop obscure-name) ,zero)
           (t ,neg))))

;; example of nif being, well, buggy:

(let ((obscure-name 'pos) (x 42))
  (nif-buggy x obscure-name 'zero 'neg)) ;; => 42

;; nif-buggy with gemsyms, to avoid unwanted variable capture

(defmacro nif. (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
             ((zerop ,g) ,zero)
             (t ,neg)))))

(let ((obscure-name 'pos) (x 42))
  (nif. x obscure-name 'zero 'neg)) ;; => POS, as expected

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
