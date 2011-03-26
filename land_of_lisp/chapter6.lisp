;; Dribble of #<IO TERMINAL-STREAM> started on NIL.

#<OUTPUT BUFFERED FILE-STREAM CHARACTER #P"chapter6.lisp">
[2]> (print "foo")

"foo" 
"foo"
[3]> (progn (print "this")
(print "is weird"))

"this" 
"is weird" 
"is weird"
[4]> (progn (prin1 "this")
(prin1 "is weird"))
"this""is weird"
"is weird"
[5]> (defun say-hello ()
(print "wie heiss du?")
(let ((name (read)))
(prin1 "Nice to meet you, ")
(prin1 name)))
SAY-HELLO
[6]> (say-hello) 

"wie heiss du?" "luis felipe""Nice to meet you, ""luis felipe"
"luis felipe"
[7]> 
(princ "this hasn't quotes")
this hasn't quotes
"this hasn't quotes"
[8]> (princ '|This is a symbol with CASE!|)
This is a symbol with CASE!
|This is a symbol with CASE!|
[9]> (princ 'and-this-a-regular-symbol)
AND-THIS-A-REGULAR-SYMBOL
AND-THIS-A-REGULAR-SYMBOL
[10]> (print "#\newline is like \n")

"#newline is like n" 
"#newline is like n"
[11]> (princ "#\newline is like \n")
#newline is like n
"#newline is like n"
[12]> (princ #\newline)

#\Newline
[13]> ;read-line reads stuff and interprets it as a string

(print (read-line))
Luis

"Luis" 
"Luis"
[14]> (defparameter *arb-code* '(cadr '(1 2 3 4)))
*ARB-CODE*
[15]> (eval *arb-code* )
2
[16]> (load "chapter5_game.lisp")
;; Loading file chapter5_game.lisp ...
*** - READ en #<INPUT BUFFERED FILE-STREAM CHARACTER #P"chapter5_game.lisp" @108>: no existe ningún paquete con el nombre
       "ITEMS"

Es posible continuar en los siguientes puntos:
ABORT          :R1      Abort main loop
Break 1 [17]> (load "chapter5_game.lisp")
;;  Loading file chapter5_game.lisp ...
*** - DEFUN: Invalid specialized parameter in method lambda list (LET ((CMD (GAME-READ))) (UNLESS (EQ (CAR CMD) 'QUIT)
      (GAME-PRINT (GAME-EVAL CMD)) (GAME-REPL))): ((CMD (GAME-READ)))
Es posible continuar en los siguientes puntos:
SKIP           :R1      skip (DEFUN GAME-REPL #)
STOP           :R2      stop loading file /home/lfborjas/workspace/land_of_lisp/chapter5_game.lisp
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort main loop
Break 2 [18]> (load "chapter5_game.lisp")
;;   Loading file chapter5_game.lisp ...
;;   Loaded file chapter5_game.lisp
T
Break 2 [18]> (game-repl)
look
You are in the living-room. A wizard snores loudly on the couch there is a door going west from here. There is a ladder
 going upstairs from here. You see whiskey on the floor. You see bucket on the floor.
walk west
You are in a beautiful garden. There is a well in front of you there is a door going east from here. You see frog on the
 floor. You see chain on the floor.
pickup chain
You are now carrying the chain
scratch head
I do not know that command.
pickup chicken
You cannot get that!
walk east
You are in the living-room. A wizard snores loudly on the couch there is a door going west from here. There is a ladder
 going upstairs from here. You see whiskey on the floor. You see bucket on the floor.
walk upstairs
You are in the attic. There is a giant welding torch in the corner there is a ladder going downstairs from here.
inventory
Items chain
walk china
You cannot go that way!
walk downstairs
You are in the living-room. A wizard snores loudly on the couch there is a door going west from here. There is a ladder
 going upstairs from here. You see whiskey on the floor. You see bucket on the floor.
pickup bucket
You are now carrying the bucket
look
You are in the living-room. A wizard snores loudly on the couch there is a door going west from here. There is a ladder
 going upstairs from here. You see whiskey on the floor.
quit
NIL
Break 2 [18]> 
[19]> 
Adiós.
