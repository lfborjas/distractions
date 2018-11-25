;;; Chapter 18 of the book: Lazy Evaluation

;; Given a set of exprs, let-over-lambda over them
;; to only evaluate when the lambda is called, and memoize
;; subsequently
(defmacro lazy (&body body)
  (let ((forced (gensym))
        (value  (gensym)))
    `(let ((,forced nil)
           (,value  nil))
       (lambda ()
         (unless ,forced
           (setf ,value (progn ,@body))
           (setf ,forced t))
         ,value))))

;; Reify a "lazy" value, e.g., a lambda created by `lazy`.
(defun force (lazy-value)
  (funcall lazy-value))

;;; LAZY LISTS LIBRARY

;; According to the book, it's "loosely" based in the Clojure implementation
;; for lazy sequences.

(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))

(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (x)
  (not (force x)))

;; Convert a regular list into a lazy list:
(defun make-lazy (lst)
  (lazy (when lst
          (cons (car lst)
                (make-lazy (cdr lst))))))

;; Lazy list to regular: take n values from the front
;; of the lazy list
(defun take (n lst)
  (unless (or (zerop n) (lazy-null lst))
    (cons (lazy-car lst)
          (take (1- n) (lazy-cdr lst)))))

;; Reify the list
(defun take-all (lst)
  (unless (lazy-null lst)
    (cons (lazy-car lst) (take-all (lazy-cdr lst)))))

;;; Example usage of lazy lists for an infinite sequence:
;; (defparameter *integers*
;;   (labels ((f (n)
;;              (lazy-cons n (f (1+ n)))))
;;     (f 1)))
;; (take 10 *integers*) => (1 2 3 4 5 6 7 8 9 10)
;; (take 5 (make-lazy '(q w e r t y u i o p))) => (Q W E R T)

(defun lazy-mapcar (fun lst)
  (lazy (unless (lazy-null lst)
          (cons (funcall fun (lazy-car lst))
                (lazy-mapcar fun (lazy-cdr lst))))))

(defun lazy-mapcan (fun lst)
  (labels ((f (lst-cur)
             (if (lazy-null lst-cur)
                 (force (lazy-mapcan fun (lazy-cdr lst)))
                 (cons (lazy-car lst-cur) (lazy (f (lazy-cdr lst-cur)))))))
    (lazy (unless (lazy-null lst)
            (f (funcall fun (lazy-car lst)))))))

(defun lazy-find-if (fun lst)
  (unless (lazy-null lst)
    (let ((x (lazy-car lst)))
      (if (funcall fun x)
          x
          (lazy-find-if fun (lazy-cdr lst))))))

(defun lazy-nth (n lst)
  (if (zerop n)
      (lazy-car lst)
      (lazy-nth (1- n) (lazy-cdr lst))))

;; Example usage of the above:
;; (take 10 (lazy-mapcar #'sqrt *integers*))
;; (1 1.4142135 1.7320508 2 2.236068 2.4494898 2.6457512 2.828427 3 3.1622777)
;;
;; Remember that mapcan is supposed to take lists from its results and return
;; an "append"-ed one.
;; (take 10 (lazy-mapcan (lambda (x)
;;                         (if (evenp x)
;;                             (make-lazy (list x))
;;                             (lazy-nil)))
;;                       *integers*))
;; (2 4 6 8 10 12 14 16 18 20)
;;
;; (lazy-find-if #'oddp (make-lazy '(2 4 6 7 8 10 11)))
;; 7
;;
;; (lazy-nth 4 (make-lazy '(a b c d e f g)))
;; E




