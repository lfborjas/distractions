;use an association list for describing the nodes in the game graph
(defparameter *nodes* '((living-room (you are in the living-room. A wizard snores loudly on the couch))
                        (garden (you are in a beautiful garden. There is a well in front of you))
                        (attic  (you are in the attic. There is a giant welding torch in the corner))))

;get the description associated with a location in an alist
(defun describe-location (location nodes) 
    (cadr (assoc location nodes)))

;describe the edges in the game graph
(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic  (living-room downstairs ladder))))

;get a textual representation of an edge
;uses quasiquoting, which is much like string interpolation in ruby/php
(defun describe-path (edge)
    `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;I could use it like this:
;>(describe-path (cadr (assoc 'garden *edges*)))
;=> (THERE IS A DOOR GOING EAST FROM HERE.)

;describe multiple paths
(defun describe-paths (location edges)
    ;apply 
    ;mapcar takes a function and a list and applies it to every member
    ;`#' foo` is shorthand for `(function foo)`
    ;common lisp has TWO namespaces (one for vars and one for functions)
    ;so you MUST say something is a function explicitly (via #' or function)
    ;whereas scheme has just one namespace. That's why scheme is a lisp-1 and clisp, a lisp-2
    
    ;append is like a join+flatten for lists
    ;apply takes a function and a list and converts this latter in parameters for the function:
    ;(append '(mary had) '(a) '(little lamb)) <=> (apply #'append '((mary had) (a) (little lamb)))
    (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;more apply examples:
;Break 7 [16]> (append '() '(1 2 3) '(4))
;(1 2 3 4)
;Break 7 [16]> (append '(() (1 2 3) (4)))
;(NIL (1 2 3) (4))
;Break 7 [16]> (apply #'append '(() (1 2 3) (4)))
;(1 2 3 4)

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (frog garden)
                                   (chain garden)))

;list the objects visible from a location
(defun objects-at (location objects object-locations)
    ;we define the private/internal function `at-loc-p`, which ends in p
    ;because it's a PREDICATE (returns a truth value)
    (labels ((at-loc-p (obj)
                (eq (cadr (assoc obj object-locations)) location)))
       ;remove-if-not is much like `reject` in ruby
      (remove-if-not #'at-loc-p objects)))

;describe all objects in a given location
;these describe-* functions are purely functional:
;1. they don't depend on the external world
;2. and they are bound to return the same outputs given the same inputs
(defun describe-objects (location objects object-locations)
    (labels  ((describe-obj (obj)
                `(you see ,obj on the floor.)))
      (apply #'append (mapcar #'describe-obj (objects-at location objects object-locations)))))

;store the current location
(defparameter *location* 'living-room)

(defun look () 
    (append (describe-location *location* *nodes*)
            (describe-paths    *location* *edges*)
            (describe-objects  *location* *objects* *object-locations*)))

(defun walk (direction)
    ;assign to next the result of finding
    ;the direction in the edges associated to a location
    ;the search term will be the cadr of every list ('cause they're of the form
    ;(location direction medium) )
    ;the search term uses a named argument, named args, like in python
    ;go at the end of the call
    (let ((next (find direction 
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
        '(you cannot go that way!))))


(defun pickup (object)
    (cond ((member object 
                    (objects-at *location* *objects* *object-locations*))
            ;[COMMON IDIOM] although the push just adds to the beginning of the locations list
            ;and this might mean multiple locations for an object (the old ones aren't erased)
            ; `assoc` retrieves the first one always, so, aside from the memory usage
            ;it's the same as replacing the values
            (push (list object 'body) *object-locations*) ; add "body" as a location for the object
             `(you are now carrying the ,object))
          (t '(you cannot get that!))))

(defun inventory ()
    (cons 'items (objects-at 'body *objects* *object-locations*)))

;an easy REPL: to quit, CTRL+c + :a
;(defun repl
;    (loop (print (eval (read)))))

;a more advanced REPL, more friendly
(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))

(defun game-read ()
    (let ((cmd (read-from-string 
                    (concatenate 'string "(" (read-line) ")" ))))
          (flet ((quote-it (x)
                    (list 'quote x)))
                (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

;this function is ridiculous: unreadable!
(defun tweak-text (lst caps lit)
    (when lst
        (let ((item (car lst))
              (rest (cdr lst)))
        (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
              ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
              ((eq item #\") (tweak-text rest caps (not lit)))
                (lit (cons item (tweak-text rest nil lit)))
              ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
              (t (cons (char-downcase item) (tweak-text rest nil nil)))))))
            
(defun game-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() "
                                            (prin1-to-string lst))
                                       'list)
                                t
                                nil)
                    'string))
     (fresh-line))
