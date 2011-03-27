#lang racket

(require (lib "defmacro.ss"))

;A simple object model, based off the one proposed in
;"teach yourself scheme in fixnum days"
;(http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-14.html)

;and my poor understanding of ruby's object model, based on these:
;http://www.rubyinside.com/a-look-into-rubys-object-model-3940.html
;http://www.slideshare.net/burkelibbey/rubys-object-model-metaprogramming-and-other-magic
;http://www.hokstad.com/ruby-object-model.html
;and the book "metaprogramming ruby":
;http://pragprog.com/titles/ppmetr/metaprogramming-ruby

(provide define-class)

(struct s-object  
    (klass [instance-vars #:auto])
    #:mutable
    #:transparent
    #:auto-value (make-hash))

(struct s-class 
    (klass instance-vars methods method-vector [superclass #:auto])
    #:mutable
    #:transparent
    #:auto-value object)

(define-macro define-class
    (lambda (superclass instance-vars . methods)
        `(create-class-proc 
            ,superclass
            ;TODO: shouldn't this be a class, not a struct?
            standard-class ;the klass: all classes are standard-classes
            (list   ,@(map (lambda (ivar) `,ivar) instance-vars))
            (list   ,@(map (lambda (method) `,(car method)) methods))
            (vector ,@(map (lambda (method) `,(cadr method)) methods)))))

(define object
    (define-class
        '() ; the superclass for object
        '() ; the instance vars for an object
         ;methods?
         ))

(define standard-class
    (define-class
        object
        '()
        '()
        ;methods?
        ))


(define make-instance
    (lambda (class . ivar-pairs)
        (let* ((ivar-list (s-class-instance-vars class))
               ;create a new instance of object
               (instance (s-object class)))
               ;remove any pair that's not part of the class list
               ;and then build the hash:
               (set-object-instance-vars!
                  instance 
                  (make-hash (filter (lambda (p) (member (car p) ivar-list)) ivar-pairs)))
               instance)))

(define create-class-proc
    (lambda (superclass klass instance-vars methods method-vector)
        (standard-class 
            superclass
            klass
            (let ((superclass-ivars 
                    (if (not (eqv? superclass object)) (s-class-instance-vars superclass) '())))
              (if (null? superclass-ivars) instance-vars
                  (delete-duplicates
                    (append instance-vars superclass-ivars))))
            methods
            method-vector)))

(define delete-duplicates
  (lambda (s)
    (if (null? s) s
        (let ((a (car s)) (d (cdr s)))
          (if (memv a d) (delete-duplicates d)
              (cons a (delete-duplicates d)))))))


(define send  
    (lambda (method instance . args)
        (let ((proc 
                   (let loop ((class (s-object-klass instance)))
                    (cond
                        ((eqv? class object) (send method-missing instance method args))
                        ((null? class) (error 'send))
                        (else 
                            (let ((m (hash-ref (s-class-methods class) method) (lambda () '())))
                            (if m 
                                m
                                (loop (s-class-superclass class)))))))))
              (apply proc instance args))))

