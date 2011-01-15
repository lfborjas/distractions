;; Dribble of #<IO TERMINAL-STREAM> started on NIL.

#<OUTPUT BUFFERED FILE-STREAM CHARACTER #P"chapter2_test.lisp">
[3]> (defvar scanner (block-scanner "metaprogramming"))
SCANNER
[4]> (funcall scanner "In ruby you can do meta")
NIL
[5]> (funcall scanner "programming too")
T
[6]> 
Adiós.
