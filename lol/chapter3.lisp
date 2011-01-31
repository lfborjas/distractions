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
