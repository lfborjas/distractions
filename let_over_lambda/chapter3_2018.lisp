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


;; a macro-defining macro:
;; finds and g!-symbols (e.g. g!result, g!first) and declares gemsyms
;; bound to them in a let, which are then available in the macro,
;; similar to Clojure's autogensym capabilities or Arc's "w-uniq"
;; https://arclanguage.github.io/ref/macro.html
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


;; (macroexpand-1
;;  `(defmacro/g! nif (expr pos zero neg)
;;     `(let ((,g!result ,expr))
;;        (cond ((plusp ,g!result) ,pos)
;;              ((zerop ,g!result) ,zero)
;;              (t ,neg)))))

;; will yield

;; (DEFMACRO NIF (EXPR POS ZERO NEG)
;;   (LET ((G!RESULT (GENSYM "RESULT")))
;;     `(LET ((,G!RESULT ,EXPR))
;;        (COND ((PLUSP ,G!RESULT) ,POS) ((ZEROP ,G!RESULT) ,ZERO) (T ,NEG)))))



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

;; Given the above, this

;; (macroexpand '(defmacro! nif (o!expr pos zero neg)
;;                  `(cond ((plusp ,g!expr) ,pos)
;;                         ((zerop ,g!expr) ,zero)
;;                         (t ,neg))))

;; yields

;; (macroexpand-1 '(DEFMACRO/G! NIF (O!EXPR POS ZERO NEG)
;;                  `(LET ,(MAPCAR #'LIST (LIST G!EXPR) (LIST O!EXPR))
;;                     ,(PROGN
;;                       `(COND ((PLUSP ,G!EXPR) ,POS) ((ZEROP ,G!EXPR) ,ZERO)
;;                              (T ,NEG))))))

;; which in turn yields:

;; (DEFMACRO NIF (O!EXPR POS ZERO NEG)
;;   (LET ((G!EXPR (GENSYM "EXPR")))
;;     `(LET ,(MAPCAR #'LIST (LIST G!EXPR) (LIST O!EXPR))
;;        ,(PROGN
;;          `(COND ((PLUSP ,G!EXPR) ,POS) ((ZEROP ,G!EXPR) ,ZERO) (T ,NEG))))))

;;; For a simpler set of macros, from the book:

;; (defmacro! square (o!x)
;;   `(* ,g!x ,g!x))

;; (macroexpand '(square (incf x)))

;;; yielding:


;; (LET ((#1=#:X660 (INCF X)))
;;   (* #1# #1#))

;; (defmacro! square2 (o!x)
;;   `(progn
;;      (format t "[~a gave ~a]~%" ',o!x ,g!x)
;;      (* ,g!x ,g!x)))

;; (macroexpand '(square2 (incf y)))

;; (LET ((#1=#:X666 #2=(INCF Y)))
;;   (PROGN (FORMAT T "[~a gave ~a]~%" '#2# #1#) (* #1# #1#)))

;; CL-USER> (square2 (incf y))
;; [(INCF Y) gave 5]
;; 25
