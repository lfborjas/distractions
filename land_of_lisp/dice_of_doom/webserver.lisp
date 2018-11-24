;;; A web server from scratch, from chapter 13 of the book

;; Helper function for decode-param: given
;; the code for a character, if valid, return it
(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
               (coerce (list c1 c2) 'string)
               :radix 16
               :junk-allowed t)))
    (if code
        (code-char code)
        default)))

;; Recursively decode http escaped params:
;; CL-USER> (decode-param "foo%3F")
;; "foo?"
;; CL-USER> (decode-param "foo+bar")
;; "foo bar"
(defun decode-param (s)
  (labels ((f (lst)
             (when lst
               (case (car lst)
                 (#\% (cons (http-char (cadr lst) (caddr lst))
                            (f (cdddr lst))))
                 (#\+ (cons #\space (f (cdr lst))))
                 (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

;; Transform a querystring into a list of cons pairs
;; CL-USER> (parse-params "name=bob&age=25&gender=male")
;; ((NAME . "bob") (AGE . "25") (GENDER . "male"))
(defun parse-params (s)
  (let ((i1 (position #\= s))
        (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s (1+ i1) i2)))
                    (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s))))

;; Given an HTTP header, extract the URL and its parameters:
;; CL-USER> (parse-url "GET /lolcats.html?a=2&b=nope HTTP/1.2")
;; ("lolcats.html" (A . "2") (B . "nope"))
(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\space s))
                      (position #\space s :from-end t)))
         (x (position #\? url)))
    (if x
        (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
        (cons url '()))))


;; Chapter 12 of the book explains other streams, including string streams, which
;; makes testing this function--meant to be used with sockets--possible:
;; (get-header (make-string-input-stream "foo: 1
;; bar: abc, 123

;; "))
;; => ((FOO . "1") (BAR . "abc, 123"))
(defun get-header (stream)
  (let* ((s (read-line stream))
         (h (let ((i (position #\: s)))
              (when i
                (cons (intern (string-upcase (subseq s 0 i)))
                      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))

;; Parse the request body:
(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-params content)))))

;; Given a request handler function, open a socket and pass requests
;; to the function, with data parsed using our above helper fns.
;; socket-server is pretty much the reason why we're using CLISP
;; instead of SBCL, as sockets are not part of ANSI Common Lisp
;; and the book favored the CLISP implementation.
(defun serve (request-handler)
  (let ((socket (socket-server 8084)))
    (unwind-protect
         (loop (with-open-stream (stream (socket-accept socket))
                 (let* ((url    (parse-url (read-line stream)))
                        (path   (car url))
                        (header (get-header stream))
                        (params (append (cdr url)
                                        (get-content-params stream header)))
                        (*standard-output* stream))
                   (funcall request-handler path header params))))
      (socket-server-close socket))))

;; ERRATA: added this function to create a more modern-browser-friendly
;; response. The book simply returned a string of html, but
;; modern browsers, firefox included (Firefox was the one used in the book)
;; don't play with those spartan responses anymore: a status line
;; and headers are now mandatory
(defun build-html-response (html)
  ;; ~% will always insert a newline, which matters because
  ;; there's a mandatory newline between headers and body.
  ;; notice how ~{ ~} will print everything in a list using
  ;; the enclosed format (hence my crazy quasiquoting ways).
  ;; Also notice how I'm using `format t` to print to stdout
  ;; which in the dynamic context of the caller is the socket stream.
  (format t "~{~a~%~}"
          `("HTTP/1.1 200 OK"
            "Content-Type: text/html"
            ""
            ,html)))

;; Little test handler:
;; Notice how the trick of only redefining *standard-output* allows us to easily
;; test in isolation in the REPL:
;; CL-USER> (hello-request-handler "lolcats" '() '())
;; Sorry, I don't know that page.
;; "Sorry, I don't know that page."
;; CL-USER> (hello-request-handler "greeting" '() '())
;; <html><form>What's your name? <input name='name'/></form></html>
;; "<html><form>What's your name? <input name='name'/></form></html>"
;; CL-USER> (hello-request-handler "greeting" '() '((name . "Bob")))
;; <html>Nice to meet you, Bob!</html>
;; NIL
(defun hello-request-handler (path header params)
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
        (if (not name)
            (build-html-response
             "<html><form>What's your name?<input name='name'/></form></html>")
            (build-html-response
             ;; use format nil to return a string, build-html-response already
             ;; prints it to stdout
             (format nil "<html>Nice to meet you, ~a!</html>" (cdr name)))))
      (princ "Sorry, I don't know that page.")))
