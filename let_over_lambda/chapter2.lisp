;chapter 2 of "let over lambda"
;http://letoverlambda.com/index.cl/guest/chap2.html
;using the function-over-let-over-lambda pattern to define a class

;this abstraction maps to classes and instances in object systems:
;a call to block-scanner returns the closure defined by the let*
;which is itself a lambda that depends on the lexical environment of 
;it.
(defun block-scanner (trigger-string)
  (let* ((trig (coerce trigger-string 'list))
         (curr trig))
     (lambda (data-string)
        (let ((data (coerce data-string 'list)))
            (dolist (c data)
                (if curr
                    (setq curr
                          (if (char= (car curr) c)
                              (cdr curr) ;next char
                              trig))))
            (not curr))))) ;return t if found 

;this abstraction maps to "shared-state": static/class variables, 
;which is just enclosing our closure in yet another one:
(let ((direction 'up))
  (defun toggle-counter-direction ()
    (setq direction
          (if (eq direction 'up)
               'down
               'up)))

  (defun counter-class ()
    (let ((counter 0))
      (lambda ()
        (if (eq direction 'up)
            (incf counter)
            (decf counter))))))


