;building a DSL

;let's say we wanted to sleep for n units of time:

;first approach: create a function that takes a multiplier and a symbol:

(defun sleep-for (value unit)
    (sleep
        (* value
            (case unit
                ((seconds) 1)
                ((minutes) 60)
                ((hours)   3600)
                ;days, miliseconds, etc
                ))))

;to use it, however, we'd need to quote the symbol, as lisp evaluates
;every argument to a function before applying it:

(sleep-for 5 'minutes)
(sleep-for 2 'seconds)
(sleep-for 1 (if super-slow-mode 'days 'minutes))

;however, a macro DOES NOT evaluate it's arguments before expanding, so we
;can write this:

(defmacro sleep-for (value unit)
    `(sleep
        (* ,value
           ,(case unit
                ((seconds) 1)
                ((minutes) 60)
                ((hours)   3600)
                ;days, miliseconds, etc
                ))))

;and we can use it like this: both things are known at compile time
(sleep-for 2 seconds)
        
;however, we can NO LONGER do this: the arguments are NOT evaluated!
(sleep-for 1 (if super-slow-mode 'days 'minutes))


;let's abstract the unit of time conversion:
(defmacro unit-of-time (value unit)
    `(* ,value
           ,(case unit
                ((seconds) 1)
                ((minutes) 60)
                ((hours)   3600)
                ;days, miliseconds, etc
                )))

;let's simulate scheme's named let:
;WHAT THE FUCK DOES THIS DOOO?
(defmacro nlet (n letargs &rest body)
    `(labels ((,n ,(mapcar #'car letargs)
               ,@body))
        (,n ,@(mapcar #'cadr letargs)))) 

;whatever it is, we use it like so:
(defun nlet-fact (n)
    (nlet fact ((n n))
        (if (zerop n)
            1
            (* n (fact (- n 1))))))

;macroexpand it!

;beware of variable capture!
;nif is like compare-to
(nif x "positive" "zero" "negative")

;The error here is that `obscure-name`
;could refer to a variable in the same
;lexical scope where this macro is expanded
;thus, returning undefined results
(defmacro nif (expr pos zero neg)
    `(let ((obscure-name ,expr))
        (cond ((plusp obscure-name) ,pos)
              ((zerop obscure-name) ,zero)
              (t ,neg))))

;alternative, capture-safe, implementation
(defmacro nif (expr pos zero neg)
   (let ((g (gensym)))
     `(let ((,g ,expr))
        (cond ((plusp ,g) ,pos)
              ((zerop ,g) ,zero)
              (t ,neg)))))

;gensyms disappear in macro expansion, that's cool

(defun g!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s) 2)
         (string= (symbol-name s)
                  "G!"
                  :start1 0
                  :end1 2))))

;this macro automatically generates gensyms for every g!symbol we have
;note that flatten is a function defined by P. Graham in "on lisp"
(defun flatten (x)
  (labels ((rec (x acc)
        (cond ((null x) acc)
              ((atom x) (cons x acc))
              (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defmacro defmacro/g! (name args &rest body)
    (let ((syms (remove-duplicates 
                    (remove-if-not #'g!-symbol-p (flatten body)))))
       `(defmacro ,name ,args
            (let ,(mapcar
                    (lambda (s)
                        `(,s (gensym ,(subseq 
                                        (symbol-name s)
                                        2))))
                    syms)
             ,@body))))

;we could, then, rewrite nif as follows:
(defmacro/g! nif (expr pos zero neg)
  `(let ((,g!result ,expr))
     (cond ((plusp ,g!result) ,pos)
           ((zerop ,g!result) ,zero)
            (t ,neg))))

;an examination of Norvig's concept of once-only

(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "O!"
                :start1 0
                :end1 2)))

;this is another PG utility
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun o!-symbol-to-g!-symbol (s)
  (symb "G!"
        (subseq (symbol-name s) 2)))


