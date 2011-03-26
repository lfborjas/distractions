;; Dribble of #<IO TERMINAL-STREAM> started on NIL.

#<OUTPUT BUFFERED FILE-STREAM CHARACTER #P"chapter4.lisp">
[2]> (defun my-length (list)
You are in the top-level Read-Eval-Print loop.
Help (abbreviated :h) = this list
Use the usual editing capabilities.
(quit) or (exit) leaves CLISP.
:break

*** - Ctrl-C: Interrupción del usuario
Es posible continuar en los siguientes puntos:
ABORT          :R1      Abort main loop
Break 1 [3]> (defun my-length (list) (if list (1+ (my-length (cdr list))) 0 ))
MY-LENGTH
Break 1 [3]> (my-length '(list with symbols))
3
Break 1 [3]> (my-length '((car (1 2 3)) (cdr (a b c)))))
2
Break 1 [3]> 
*** - READ en
       #<INPUT CONCATENATED-STREAM #<INPUT STRING-INPUT-STREAM>
         #<IO TWO-WAY-STREAM
           #<IO ECHO-STREAM #<IO TERMINAL-STREAM> #<OUTPUT BUFFERED FILE-STREAM CHARACTER #P"chapter4.lisp">>
           #<OUTPUT BROADCAST-STREAM #<IO TERMINAL-STREAM> #<OUTPUT BUFFERED FILE-STREAM CHARACTER #P"chapter4.lisp">>>>
      : un objeto no puede comenzar por #\)
Es posible continuar en los siguientes puntos:
ABORT          :R1      Abort debug loop
ABORT          :R2      Abort main loop
Break 2 [4]> (eq '() nil)
T
Break 2 [4]> (eq '() ())
T
Break 2 [4]> (eq '() 'nil)
T
Break 2 [4]> (if '(1 2) "the list has stuff in it" "nope, empty")
"the list has stuff in it"
Break 2 [4]> (if '() "the list has stuff in it" "nope, empty")
"nope, empty"
Break 2 [4]> (if (oddp 5) 'odd 'even)
ODD
Break 2 [4]> (oddp)

*** - EVAL: no se han entregado suficientes argumentos a ODDP: (ODDP)
Es posible continuar en los siguientes puntos:
ABORT          :R1      Abort debug loop
ABORT          :R2      Abort debug loop
ABORT          :R3      Abort main loop
Break 3 [5]> (if oddp 'exists 'does-not)

*** - EVAL: la variable ODDP no tiene ningún valor
Es posible continuar en los siguientes puntos:
USE-VALUE      :R1      You may input a value to be used instead of ODDP.
STORE-VALUE    :R2      You may input a new value for ODDP.
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
ABORT          :R6      Abort main loop
Break 4 [6]> (if 'oddp 'exists 'does-not)
EXISTS
Break 4 [6]> (if oddp 'exists 'does-not)

*** - EVAL: la variable ODDP no tiene ningún valor
Es posible continuar en los siguientes puntos:
USE-VALUE      :R1      You may input a value to be used instead of ODDP.
STORE-VALUE    :R2      You may input a new value for ODDP.
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
ABORT          :R6      Abort debug loop
ABORT          :R7      Abort main loop
Break 5 [7]> oddp

*** - EVAL: la variable ODDP no tiene ningún valor
Es posible continuar en los siguientes puntos:
USE-VALUE      :R1      You may input a value to be used instead of ODDP.
STORE-VALUE    :R2      You may input a new value for ODDP.
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
ABORT          :R6      Abort debug loop
ABORT          :R7      Abort debug loop
ABORT          :R8      Abort main loop
Break 6 [8]> (oddp 5)
T
Break 6 [8]> (defun a 5)

*** - DEFUN: Sólo se permiten las listas lambda con puntos en la macros, no aquí: 5
Es posible continuar en los siguientes puntos:
ABORT          :R1      Abort debug loop
ABORT          :R2      Abort debug loop
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
ABORT          :R6      Abort debug loop
ABORT          :R7      Abort main loop
Break 7 [9]> (if (oddp 5) (progn (setf *was-odd* t) 'odd!) 'even!)
ODD!
Break 7 [9]> *was-odd* 
T
Break 7 [9]> ;you use progn to have more than one action in a branch


*** - Ctrl-C: Interrupción del usuario
Es posible continuar en los siguientes puntos:
ABORT          :R1      Abort debug loop
ABORT          :R2      Abort debug loop
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
ABORT          :R6      Abort debug loop
ABORT          :R7      Abort debug loop
ABORT          :R8      Abort main loop
Break 8 [10]> (defun number-stuff (n) (when (evenp n) (setf *even!* t) 'odd!))
NUMBER-STUFF
Break 8 [10]> (number-stuff 5)
NIL
Break 8 [10]> (number-stuff 4)
ODD!
Break 8 [10]> (defun number-stuff (n) (when (evenp n) (setf *even!* t) 'even!))
NUMBER-STUFF
Break 8 [10]> (number-stuff 4)
EVEN!
Break 8 [10]> (defun number-stuff (n) (unless (evenp n) (setf *was-odd* t) 'odd!))
NUMBER-STUFF
Break 8 [10]> (number-stuff 4)
NIL
Break 8 [10]> (number-stuff 5)
ODD!
Break 8 [10]> (defun more-number-stuff (n) (cond ((evenp n) (setf *was-odd* f) 'even) ((oddp n) (setf *was-odd* t) 'odd)))
MORE-NUMBER-STUFF
Break 8 [10]> (more-number-stuff 5)
ODD
Break 8 [10]> *was-odd* 
T
Break 8 [10]> (more-number-stuff 4)

*** - EVAL: la variable F no tiene ningún valor
Es posible continuar en los siguientes puntos:
USE-VALUE      :R1      You may input a value to be used instead of F.
STORE-VALUE    :R2      You may input a new value for F.
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
ABORT          :R6      Abort debug loop
ABORT          :R7      Abort debug loop
ABORT          :R8      Abort debug loop
ABORT          :R9      Abort debug loop
ABORT          :R10     Abort debug loop
ABORT          :R11     Abort main loop
Break 9 [11]> (defun more-number-stuff (n) (cond ((evenp n) (setf *was-odd* nil) 'even) ((oddp n) (setf *was-odd* t) 'odd)))
MORE-NUMBER-STUFF
Break 9 [11]> (more-number-stuff 4)
EVEN
Break 9 [11]> *was-odd* 
NIL
Break 9 [11]> (more-number-stuff 5)
ODD
Break 9 [11]> *was-odd* 
T
Break 9 [11]> (defun pudding-eater (person)
(case person
((henry)
(setf *arch-enemy* 'stupid-lisp-alien)
'(curse you lisp alien – you ate my pudding))
((johnny) (setf *arch-enemy* 'useless-old-johnny)
'(i hope you choked on my pudding johnny))
(otherwise '(why you eat my pudding stranger ?))))
PUDDING-EATER
Break 9 [11]> (pudding-eater 'henry)
(CURSE YOU LISP ALIEN – YOU ATE MY PUDDING)
Break 9 [11]> (pudding-eater 'johnny)
(I HOPE YOU CHOKED ON MY PUDDING JOHNNY)
Break 9 [11]> (pudding-eater 'george-clooney)
(WHY YOU EAT MY PUDDING STRANGER ?)
Break 9 [11]> (mod 5 5)
0
Break 9 [11]> (mod 15 5)
0
Break 9 [11]> (or (oddp 4) (set *even?* t)) ; we use short-circuit to eval stuff

*** - EVAL: la variable *EVEN?* no tiene ningún valor
Es posible continuar en los siguientes puntos:
USE-VALUE      :R1      You may input a value to be used instead of *EVEN?*.
STORE-VALUE    :R2      You may input a new value for *EVEN?*.
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
ABORT          :R6      Abort debug loop
ABORT          :R7      Abort debug loop
ABORT          :R8      Abort debug loop
ABORT          :R9      Abort debug loop
ABORT          :R10     Abort debug loop
ABORT          :R11     Abort debug loop
ABORT          :R12     Abort main loop
Break 10 [12]> (or (oddp 4) (setf *even?* t)) ; we use short-circuit to eval stuff
T
Break 10 [12]> (member (2 1 3 5))

*** - EVAL: 2 is not a function name; try using a symbol instead
Es posible continuar en los siguientes puntos:
USE-VALUE      :R1      You may input a value to be used instead.
ABORT          :R2      Abort debug loop
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
ABORT          :R6      Abort debug loop
ABORT          :R7      Abort debug loop
ABORT          :R8      Abort debug loop
ABORT          :R9      Abort debug loop
ABORT          :R10     Abort debug loop
ABORT          :R11     Abort debug loop
ABORT          :R12     Abort main loop
Break 11 [13]> (member 1 (2 1 3 5))

*** - EVAL: 2 is not a function name; try using a symbol instead
Es posible continuar en los siguientes puntos:
USE-VALUE      :R1      You may input a value to be used instead.
ABORT          :R2      Abort debug loop
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
ABORT          :R6      Abort debug loop
ABORT          :R7      Abort debug loop
ABORT          :R8      Abort debug loop
ABORT          :R9      Abort debug loop
ABORT          :R10     Abort debug loop
ABORT          :R11     Abort debug loop
ABORT          :R12     Abort debug loop
ABORT          :R13     Abort main loop
Break 12 [14]> (member 1 '(2 1 3 5))
(1 3 5)
Break 12 [14]> (defun miembro (x lista) (if (or (eq x (car lista)) ()) (cdr lista) (miembro x (cdr lista))))
MIEMBRO
Break 12 [14]> (miembro 1 '(2 1 3 5))
(3 5)
Break 12 [14]> (miembro 4 '(2 1 3 5))
Break 12 [14]> (cdr '())
NIL
Break 12 [14]> (setf y '())
NIL
Break 12 [14]> (eq y nil)
T
Break 12 [14]> (member 1 '(2 3 5))
NIL
Break 12 [14]> (setf l '(1 2 4))
(1 2 4)
Break 12 [14]> (car l)
1
Break 12 [14]> l
(1 2 4)
Break 12 [14]> (cdr l)
(2 4)
Break 12 [14]> l
(1 2 4)
Break 12 [14]> (or (eq 1 (car '(2 3))) ())
NIL
Break 12 [14]> (or (eq 2 (car '(2 3))) ())
T
Break 12 [14]> (defun miembro (x lista) (if (or (eq x (car lista)) ()) lista (miembro x (cdr lista))))
MIEMBRO
Break 12 [14]> (miembro 1 '(2 1 3 5))
(1 3 5)
Break 12 [14]> (miembro 4 '(2 1 3 5))
Break 12 [14]> (or (eq 4 (car '(1 2 3 5))) ())
NIL
Break 12 [14]> (or (eq 4 (car (cdr '(1 2 3 5)))) ())
NIL
Break 12 [14]> (or (eq 4 (car (cdr (cdr '(1 2 3 5)))) ()))

*** - EVAL: se han entregado demasiados argumentos a EQ: (EQ 4 (CAR (CDR (CDR '(1 2 3 5)))) NIL)
Es posible continuar en los siguientes puntos:
ABORT          :R1      Abort debug loop
ABORT          :R2      Abort debug loop
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
ABORT          :R6      Abort debug loop
ABORT          :R7      Abort debug loop
ABORT          :R8      Abort debug loop
ABORT          :R9      Abort debug loop
ABORT          :R10     Abort debug loop
ABORT          :R11     Abort debug loop
ABORT          :R12     Abort debug loop
ABORT          :R13     Abort main loop
Break 13 [15]> (or (eq 4 (car (cdr (cdr '(1 2 3 5))))) ())
NIL
Break 13 [15]> (or (eq 4 (car (cdr (cdr (cdr '(1 2 3 5)))))) ())
NIL
Break 13 [15]> (or (eq 4 (car (cdr (cdr (cdr (cdr '(1 2 3 5))))))) ())
NIL
Break 13 [15]> (car ())
NIL
Break 13 [15]> (defun miembro (x lista) (if (or (eq x (car lista)) (eq lista nil)) lista (miembro x (cdr lista))))
MIEMBRO
Break 13 [15]> (miembro 5 '(1 3 5))
(5)
Break 13 [15]> (miembro 5 '(1 3 5 6 7))
(5 6 7)
Break 13 [15]> (miembro 4 '(1 3 5 6 7))
NIL
Break 13 [15]> (find-if #'evenp '(1 3 5))
NIL
Break 13 [15]> (find-if #'evenp '(1 3 5 6 4))
6
Break 13 [15]> (find-if #'evenp '(1 3 5 2 6 4)) ;returns the FIRST value for which f returns a non-nil val
2
Break 13 [15]> (setf *fruit* 'apple)
APPLE
Break 13 [15]> (eq *fruit* 'apple)
T
Break 13 [15]> (eq *fruit* 'orange)
NIL
Break 13 [15]> (eq *fruit* 'apple) ;eq is for comparing symbols
T
Break 13 [15]> (equal 1 1)
T
Break 13 [15]> (equal 'range 'range)
T
Break 13 [15]> (equal 'range 'range); equal is for everything else
T
Break 13 [15]> (eql *fruit* 'apple)
T
Break 13 [15]> (eql #\a #\a)
T
Break 13 [15]> (eql 2.0 2.0) ;eql is a hybrid: can compare symbols, numbers and chars
T
Break 13 [15]> (eql "str" "str")
NIL
Break 13 [15]> (eql (list a b c) (list a b c))

*** - EVAL: la variable A no tiene ningún valor
Es posible continuar en los siguientes puntos:
USE-VALUE      :R1      You may input a value to be used instead of A.
STORE-VALUE    :R2      You may input a new value for A.
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
ABORT          :R6      Abort debug loop
ABORT          :R7      Abort debug loop
ABORT          :R8      Abort debug loop
ABORT          :R9      Abort debug loop
ABORT          :R10     Abort debug loop
ABORT          :R11     Abort debug loop
ABORT          :R12     Abort debug loop
ABORT          :R13     Abort debug loop
ABORT          :R14     Abort debug loop
ABORT          :R15     Abort debug loop
ABORT          :R16     Abort main loop
Break 14 [16]> (eql '(list a b c) '(list a b c))
NIL
Break 14 [16]> (eql '(list a b c) '(list a b c)) ; as you see, eql is useless for lists
NIL
Break 14 [16]> (equal '(list a b c) '(list a b c)) ; as you see, eql is useless for lists
T
Break 14 [16]> (eq '(list a b c) '(list a b c)) ; as you see, eql is useless for lists
NIL
Break 14 [16]> (equalp "OH MY GOD",
*** - READ: la coma es inválida fuera del `backquote'
Es posible continuar en los siguientes puntos:
ABORT          :R1      Abort debug loop
ABORT          :R2      Abort debug loop
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
ABORT          :R6      Abort debug loop
ABORT          :R7      Abort debug loop
ABORT          :R8      Abort debug loop
ABORT          :R9      Abort debug loop
ABORT          :R10     Abort debug loop
ABORT          :R11     Abort debug loop
ABORT          :R12     Abort debug loop
ABORT          :R13     Abort debug loop
ABORT          :R14     Abort debug loop
ABORT          :R15     Abort main loop
Break 15 [17]> (equalp "OH MY GOD" "OH my god")
T
Break 15 [17]> (equalp 2.0 2);equalp is for stuff that is the same but doesn't quite look so...
T
Break 15 [17]> (= 1 1)
T
Break 15 [17]> (= "a" "a")

*** - =: "a" is not a number
Es posible continuar en los siguientes puntos:
USE-VALUE      :R1      You may input a value to be used instead.
ABORT          :R2      Abort debug loop
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
ABORT          :R6      Abort debug loop
ABORT          :R7      Abort debug loop
ABORT          :R8      Abort debug loop
ABORT          :R9      Abort debug loop
ABORT          :R10     Abort debug loop
ABORT          :R11     Abort debug loop
ABORT          :R12     Abort debug loop
ABORT          :R13     Abort debug loop
ABORT          :R14     Abort debug loop
ABORT          :R15     Abort debug loop
ABORT          :R16     Abort debug loop
ABORT          :R17     Abort main loop
Break 16 [18]> (= 2.0 2) ; = is for numbers
T
Break 16 [18]> (string-equal "a" "b") ;string-equal and char-equal are for what is obvious
NIL
Break 16 [18]> 
Break 15 [17]> 
*** - Ctrl-C: Interrupción del usuario
Es posible continuar en los siguientes puntos:
ABORT          :R1      Abort debug loop
ABORT          :R2      Abort debug loop
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
ABORT          :R6      Abort debug loop
ABORT          :R7      Abort debug loop
ABORT          :R8      Abort debug loop
ABORT          :R9      Abort debug loop
ABORT          :R10     Abort debug loop
ABORT          :R11     Abort debug loop
ABORT          :R12     Abort debug loop
ABORT          :R13     Abort debug loop
ABORT          :R14     Abort debug loop
ABORT          :R15     Abort debug loop
ABORT          :R16     Abort main loop
Break 16 [19]> 
Break 15 [17]> (exit)
Adiós.
