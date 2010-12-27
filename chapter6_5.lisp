;; Dribble of #<IO TERMINAL-STREAM> started on NIL.

#<OUTPUT BUFFERED FILE-STREAM CHARACTER #P"chapter6_5.lisp">
[2]> (remove-if-not (lambda (n) (eq 0 (mod n))) '(1 2 3 4 5))

*** - EVAL: no se han entregado suficientes argumentos a MOD: (MOD N)
Es posible continuar en los siguientes puntos:
ABORT          :R1      Abort main loop
Break 1 [3]> (remove-if-not (lambda (n) (eq 0 (mod n 2))) '(1 2 3 4 5))
(2 4)
Break 1 [3]> 
*** - Ctrl-C: Interrupción del usuario
Es posible continuar en los siguientes puntos:
** - Continuable Error
PRINT: Interrupción del usuario
Si continúa (tecleando `continue'): Continue execution
The following restarts are also available:
ABORT          :R1      Abort debug loop
ABORT          :R2      Abort main loop
Break 2 [4]> 
ABORT          :R1      Abort debug loop
ABORT          :R2      Abort main loop
Break 2 [5]> 
Break 1 [3]> 
[6]> 
Adiós.
