;;; Chapter 17 of the book; Domain Specific Languages

;; Prints an HTML/XML tag:
;; CL-USER> (print-tag 'mytag '((color . blue) (height . 9)) t)
;; </mytag color="BLUE" height="9">
;; NIL
(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
          (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
        alst)
  (princ #\>)
  ; returning nil since we want this for the side-effects only
  nil)

;; Helper macro for macros, defined in chapter 16:
;; simpler let when only one value will be bound:
;;
;;
;; (macroexpand '(let1 foo (+ 2 3)
;;                (* foo foo)))
;; (LET ((FOO (+ 2 3))) (* FOO FOO))
(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

;; Helper macro for macros, defined in chapter 16:
;; takes a value and branches if it is a list or empty.
;; Notice that it's intentionally anaphoric, introducing
;; head and tail for the calling code to use,
;; but uses a gensym to avoid accidental variable capture,
;; _and_ repeated execution of `val`:
;;
;; (macroexpand '(split '(2 3)
;;                (+ x head)
;;                nil))
;; (LET ((#1=#:G40677 '(2 3)))
;;   (IF #1#
;;       (LET ((HEAD (CAR #1#))
;;             (TAIL (CDR #1#)))
;;         (+ X HEAD)) NIL))
(defmacro split (val yes no)
  (let1 g (gensym)
        `(let1 ,g ,val
               (if ,g
                   (let ((head (car ,g))
                         (tail (cdr ,g)))
                     ,yes)
                   ,no))))

;; Helper fn for macros, defined in chapter 16,
;; splits even-length lists:
;; CL-USER> (pairs '(a b c d e f))
;; ((A . B) (C . D) (E . F))
(defun pairs (lst)
  (labels ((f (lst acc)
             (split lst
                    (if tail
                        (f (cdr tail) (cons (cons head (car tail)) acc))
                        (reverse acc))
                    (reverse acc))))
    (f lst nil)))

;; Inspired by the `tag` macro in Arc, by PG:
;; given a tag name, attributes and a body, it surrounds the body
;; with the correct tags. Notice it uses the `print-tag` and
;; `pairs` utility fns.
;;
;; (macroexpand '(tag circle (color 'blue height (+ 4 5))))
;; (PROGN
;;  (PRINT-TAG 'CIRCLE (LIST (CONS 'COLOR 'BLUE) (CONS 'HEIGHT (+ 4 5))) NIL)
;;  (PRINT-TAG 'CIRCLE NIL T))
;;
;; (macroexpand '(tag circle (color 'blue size 'small)
;;                (tag text ())
;;                (tag text (color 'black))))
;; (PROGN (PRINT-TAG 'CIRCLE (LIST (CONS 'COLOR 'BLUE) (CONS 'SIZE 'SMALL)) NIL)
;;        (TAG TEXT NIL)
;;        (TAG TEXT (COLOR 'BLACK))
;;        (PRINT-TAG 'CIRCLE NIL T))
;;
;; and calling the above would print:
;; <circle color="BLUE" size="SMALL">
;;     <text></text>
;;     <text color="BLACK"></text>
;; </circle>
(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
                     (list ,@ (mapcar (lambda (x)
                                        `(cons ',(car x) ,(cdr x)))
                                      (pairs atts)))
                     nil)
          ,@body
          (print-tag ',name nil t)))

;; some convenience macros for common html tasks:
;; CL-USER> (html (body (princ "hello")))
;; <html><body>hello</body></html>
(defmacro html (&body body)
  `(tag html ()
     ,@body))

(defmacro body (&body body)
  `(tag body ()
     ,@body))
