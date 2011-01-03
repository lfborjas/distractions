;; Dribble of #<IO TERMINAL-STREAM> started on NIL.

#<OUTPUT BUFFERED FILE-STREAM CHARACTER #P"chapter7.lisp">
Break 1 [5]> ;this is a regular list
(cons 1 (cons 2 (cons 3 nil)))
(1 2 3)
Break 1 [5]> ;and this, a dotted list: one where nil isn't where it's supposed to
(cons 1 (cons 2 3))
(1 2 . 3)
Break 1 [5]> ;this is a list using dot notation, using the dot as a sort of "data-mode-cons"
'(1 . (2 . (3 . nil)))
(1 2 3)
Break 1 [5]> ;dotted lists are useful for making pairs:
(cons 2 3)
(2 . 3)
Break 1 [5]> ;it's more efficient and convenient than the alternative:
(cons 2 (cons 3 nil))
(2 3)
Break 1 [5]> ;because it only needs to allocate ONE cons cell, not two
;now, we're gonna do circular lists; but first, warn the REPL that circular references may ensue:
(setf *print-circle* t)
T
Break 1 [5]> ;one way to create a circular list (one where the last place of the last cell points to the first one) is this:
(defparameter foo '(1 2 3))
FOO
Break 1 [5]> (setf (cddr foo) foo) ;point the last cdr to the list itself
#1=(1 2 . #1#)
Break 1 [5]> (setf (cdddr foo) foo) ;point the last cdr to the list itself (the last one was the second last...)
#1=(1 . #1#)
Break 1 [5]> (defparameter foo '(1 2 3))
FOO
Break 1 [5]> (setf (cdddr foo) foo) ;point the last cdr to the list itself (the last one was the second last...)
#1=(1 2 3 . #1#)
Break 1 [5]> ;now, association lists (alist)
(defparameter *drink-order* '((bill . double-espresso)
                              (lisa . small-drip-coffee)
                              (john . medium-latte)))
*DRINK-ORDER*
Break 1 [5]> ;to look-up: assoc
(assoc 'lisa *drink-order* )
(LISA . SMALL-DRIP-COFFEE)
Break 1 [5]> ;use push to "update" values, works because the FIRST occurence is the one fetched by assoc
(push '(lisa . absinthe) *drink-order* )
((LISA . ABSINTHE) (BILL . DOUBLE-ESPRESSO) (LISA . SMALL-DRIP-COFFEE) (JOHN . MEDIUM-LATTE))
Break 1 [5]> (assoc 'lisa *drink-order* )
(LISA . ABSINTHE)
Break 1 [5]> ;alists are good for little dictionaries, but are space-inefficient because data *never* goes away (though it could be good for versioning)
;you could represent a tree (which is a aciclic directed graph), like this:
(defparameter *house* '((walls (mortar (cement)

Commands may be abbreviated as shown in the second column.
COMMAND        ABBR     DESCRIPTION
Help           :h, ?    this command list
Error          :e       Print the last error message
Inspect        :i       Inspect the last error
Abort          :a       abort to the next recent input loop
Unwind         :uw      abort to the next recent input loop
Reset          :re      toggle *PACKAGE* and *READTABLE* between the
                          local bindings and the sane values
Quit           :q       quit to the top-level input loop
Mode-1         :m1      inspect all the stack elements
Mode-2         :m2      inspect all the frames
Mode-3         :m3      inspect only lexical frames
Mode-4         :m4      inspect only EVAL and APPLY frames (default)
Mode-5         :m5      inspect only APPLY frames
Where          :w       inspect this frame
Up             :u       go up one frame, inspect it
Top            :t       go to top frame, inspect it
Down           :d       go down one frame, inspect it
Bottom         :b       go to bottom (most recent) frame, inspect it
Backtrace-1    :bt1     list all stack elements
Backtrace-2    :bt2     list all frames
Backtrace-3    :bt3     list all lexical frames
Backtrace-4    :bt4     list all EVAL and APPLY frames
Backtrace-5    :bt5     list all APPLY frames
Backtrace      :bt      list stack in current mode
Backtrace-l    :bl      list stack in current mode.
                          Limit of frames to print will be prompted for.
Frame-limit    :fl      set the frame-limit. This many frames will
                          be printed in a backtrace at most.
Break+         :br+     set breakpoint in EVAL frame
Break-         :br-     disable breakpoint in EVAL frame
Redo           :rd      re-evaluate form in EVAL frame
Return         :rt      leave EVAL frame, prescribing the return values
Es posible continuar en los siguientes puntos:
ABORT          :R1      Abort main loop
(defparameter *house* '((walls (mortar (cement)
                                       (water)
                                       (sand))
                                (bricks))
                         (windows (glass)
                                  (frame)
                                  (curtains))
                         (roof (shingles)
                               (chimney))))

)
)
)

*** - Ctrl-C: Interrupción del usuario
Es posible continuar en los siguientes puntos:
ABORT          :R1      Abort debug loop
ABORT          :R2      Abort main loop
Break 2 [6]> ;graphs could be stored as two alists: one for the edges and one for the nodes, and visualized with something like graphviz
;;WARNING: the upcoming FORKS (system calls) are only possible from within the CLISP REPL!
;generate dot valid names:
(defun dot-name (exp) 
 (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))
DOT-NAME
Break 2 [6]> (dot-name 'living-room)
"LIVING_ROOM"
Break 2 [6]> (dot-name 'foo!*$)
"FOO___"
Break 2 [6]> ;substitute if works like this: (substitute-if REPLACEMENT TEST-FUNCTION EXPRESSION)
(substitute-if #\e #'digit-char-p "Ich bin ein 1337 hack3r")
"Ich bin ein eeee hacker"
Break 2 [6]> (substitute-if #\e #'digit-char-p "Ich bin ein l33t hack3r")
"Ich bin ein leet hacker"
Break 2 [6]> ;can be used on lists, too!
(substitute-if 0 #'oddp '(1 2 3 4 5 6 7))
(0 2 0 4 0 6 0)
Break 2 [6]> ;and complement takes a function and returns another which behaves like the original's opposite:
((complement #'oddp) 2)

*** - EVAL: (COMPLEMENT #'ODDP) is not a function name; try using a symbol instead
Es posible continuar en los siguientes puntos:
USE-VALUE      :R1      You may input a value to be used instead.
ABORT          :R2      Abort debug loop
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort main loop
Break 3 [7]> ('(complement #'oddp) 2)

*** - EVAL: '(COMPLEMENT #'ODDP) is not a function name; try using a symbol instead
Es posible continuar en los siguientes puntos:
USE-VALUE      :R1      You may input a value to be used instead.
ABORT          :R2      Abort debug loop
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort main loop
Break 4 [8]> (apply (complement #'oddp) 2)

*** - APPLY: dotted argument list given to ODDP : 2
Es posible continuar en los siguientes puntos:
ABORT          :R1      Abort debug loop
ABORT          :R2      Abort debug loop
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort main loop
Break 5 [9]> (apply (complement #'oddp) '2)

*** - APPLY: dotted argument list given to ODDP : 2
Es posible continuar en los siguientes puntos:
ABORT          :R1      Abort debug loop
ABORT          :R2      Abort debug loop
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
ABORT          :R6      Abort main loop
Break 6 [10]> (map (complement #'oddp) '(1 2 3))

*** - EVAL: no se han entregado suficientes argumentos a MAP: (MAP (COMPLEMENT #'ODDP) '(1 2 3))
Es posible continuar en los siguientes puntos:
ABORT          :R1      Abort debug loop
ABORT          :R2      Abort debug loop
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
ABORT          :R6      Abort debug loop
ABORT          :R7      Abort main loop
Break 7 [11]> 
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
Break 8 [12]> (funcall (complement #'oddp) 2)
T
Break 8 [12]> (funcall (complement #'alphanumericp) 2)

*** - ALPHANUMERICP: argument 2 is not a character
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
ABORT          :R10     Abort main loop
Break 9 [13]> (funcall (complement #'alphanumericp) '2)

*** - ALPHANUMERICP: argument 2 is not a character
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
ABORT          :R11     Abort main loop
Break 10 [14]> (funcall (complement #'alphanumericp) a)

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
ABORT          :R13     Abort main loop
Break 11 [15]> (funcall (complement #'alphanumericp) 'a)

*** - ALPHANUMERICP: argument A is not a character
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
Break 12 [16]> (funcall (complement #'alphanumericp) #\a)
NIL
Break 12 [16]> (funcall (complement #'alphanumericp) #\2)
NIL
Break 12 [16]> ;now, generate the labels for the nodes: truncate if it's too long
(defparameter *max-label-length* 30)
*MAX-LABEL-LENGTH*
Break 12 [16]> (defun dot-label (exp)
                 (if exp
                   (let ((s (write-to-string exp :pretty nil)))
                      (if (> (length s) *max-label-length*) 
                          (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
                          s))
                   ""))
DOT-LABEL
Break 12 [16]> ;write-to-string can take keyword parameters, in this case, we send `:pretty nil` to avoid pretty printing
(write-to-string 'look-thither)
"LOOK-THITHER"
Break 12 [16]> (write-to-string 'look-thither :pretty nil)
"LOOK-THITHER"
Break 12 [16]> ;now, let's write the nodes in a dot-suitable way:
(defun nodes->dot (nodes)
 (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))
NODES->DOT
Break 12 [16]> ;mapc is like scheme's for-each: is like map but doesn't return the list
(defparameter *wizard-nodes* '((living-room (you are blah))
                               (garten (der seele))
                               (graebern (der affs))))
*WIZARD-NODES*
Break 12 [16]> (nodes->dot *wizard-nodes* )
LIVING_ROOM[label="(LIVING-ROOM (YOU ARE BLAH))"];
GARTEN[label="(GARTEN (DER SEELE))"];
GRAEBERN[label="(GRAEBERN (DER AFFS))"];
((LIVING-ROOM (YOU ARE BLAH)) (GARTEN (DER SEELE)) (GRAEBERN (DER AFFS)))
Break 12 [16]> ;it's interesting to note that we depend on the SIDE-EFFECTS of our nodes->dot function...
;now, the edges to dot function:
(defun edges->dot (edges)
 (mapc (lambda (node)
        (mapc (lambda (edge)
                (fresh-line)
                (princ (dot-name (car node)))
                (princ "->")
                (princ (dot-name (car edge)))
                (princ ("[label=\"")
                (princ (dot-label (cdr edge)))
                (princ ("\"];"))
              (cdr node)))
         edges))
)

*** - Ctrl-C: Interrupción del usuario
Es posible continuar en los siguientes puntos:
ABORT          :R1      Abort debug loop
ABORT          :R2      Abort debug loop
** - Continuable Error
PRINT: Interrupción del usuario
Si continúa (tecleando `continue'): Continue execution
The following restarts are also available:
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
Break 13 [17]> (defun edges->dot (edges)
 (mapc (lambda (node)
        (mapc (lambda (edge)
                (fresh-line)
                (princ (dot-name (car node)))
                (princ "->")
                (princ (dot-name (car edge)))
                (princ "[label=\"")
                (princ (dot-label (cdr edge)))
                (princ "\"];"))
              (cdr node)))
         edges))
EDGES->DOT
Break 13 [17]> 
(defparameter *wizard-edges* '((living-room (garten west-door)
                                                           (graebern upstairs ladder))
                                              (garten (living-room east door))
                                              (graebern (living-room downstairs ladder))))
*WIZARD-EDGES*
Break 13 [17]> (edges->dot *wizard-edges* )
LIVING_ROOM->GARTEN[label="(WEST-DOOR)"];
LIVING_ROOM->GRAEBERN[label="(UPSTAIRS LADDER)"];
GARTEN->LIVING_ROOM[label="(EAST DOOR)"];
GRAEBERN->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];
((LIVING-ROOM (GARTEN WEST-DOOR) (GRAEBERN UPSTAIRS LADDER)) (GARTEN (LIVING-ROOM EAST DOOR))
 (GRAEBERN (LIVING-ROOM DOWNSTAIRS LADDER)))
Break 13 [17]> ;now, print the whole graph:
(defun graph->dot (nodes edges)
 (princ "digraph{"
 (nodes->dot nodes)
 (edges->dot edges)
 (princ "}"))

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
ABORT          :R14     Abort main loop
Break 14 [18]> (defun graph->dot (nodes edges)
 (princ "digraph{")
 (nodes->dot nodes)
 (edges->dot edges)
 (princ "}"))
GRAPH->DOT
Break 14 [18]> (graph->dot *wizard-nodes* *wizard-edges* )
digraph{
LIVING_ROOM[label="(LIVING-ROOM (YOU ARE BLAH))"];
GARTEN[label="(GARTEN (DER SEELE))"];
GRAEBERN[label="(GRAEBERN (DER AFFS))"];
LIVING_ROOM->GARTEN[label="(WEST-DOOR)"];
LIVING_ROOM->GRAEBERN[label="(UPSTAIRS LADDER)"];
GARTEN->LIVING_ROOM[label="(EAST DOOR)"];
GRAEBERN->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];}
"}"
Break 14 [18]> ;now, we want to put this into a file and generate the graph:
(defun dot->png (fname thunk)
 (with-open-file (*standard-output*
                  fname
                  :direction :output
                  :if-exists :supersede)
   (funcall thunk))
 (ext:shell (concatenate 'string "dot -Tpng -O " fname)))
DOT->PNG
Break 14 [18]> ;the "thunk" is a nullary function: a lazy evaluation, so we wrap the graph->dot in a thunk to call it when dot->png really needs it
;the first parameter to `with-open-file` is a stream to use later
;keyword-symbols in lisp are like symbols in ruby or atoms in erlang: they always means themselves:
:cigar
:CIGAR
Break 14 [18]> (let ((:cigar 5)) :cigar)

*** - LET: :CIGAR is a constant, may not be used as a variable
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
ABORT          :R16     Abort main loop
Break 15 [19]> ;*standard-output* is the repl output, but in the with-open-file function we supersede it with the stream that will write directly to the file. The interesting is, this superseding is a LEXICAL SCOPING: it will be also superseded in the funcall to the thunk!! (thus bypassing the normal console printing and going directly to the file)
;now the magic: wrap the original function in a thunk and pass it to the other one:
(defun graph->png (fname nodes edges)
 (dot->png fname
           (lambda ()
             (graph->dot nodes edges))))
GRAPH->PNG
Break 15 [19]> (graph->png "wizard.dot" *wizard-nodes* *wizard-edges* )
0
Break 15 [19]> ;now, go one step further: undirected graphs
(defun udges->dot (edges)
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                    (unless (assoc (car edge) (cdr list))
                      (fresh-line)
                      (princ (dot-name (caar lst)))
                      (princ "--")
                      (princ (dot-name (car edge)))
                      (princ "[label=\"")
                      (princ (dot-label (cdr edge)))
                      (princ "\"];")))
                   (cdar lst)))
             edges))
UDGES->DOT
Break 15 [19]> (defun ugraph->dot (nodes edges)
                 (princ "graph{")
                 (nodes->dot nodes)
                 (udges->dot edges)
                 (princ "}"))
UGRAPH->DOT
Break 15 [19]> (defun ugraph->png (fname nodes edges)
                 (dot->png fname
                           (lambda ()
                            (ugraph->dot nodes edges))))
UGRAPH->PNG
Break 15 [19]> ;the maplist function works with subsequend cdrs, instead of just the current item:
(maplist #'print '(a b c))

(A B C) 
(B C) 
(C) 
((A B C) (B C) (C))
Break 15 [19]> (map #'print '(a b c))

*** - EVAL: no se han entregado suficientes argumentos a MAP: (MAP #'PRINT '(A B C))
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
Break 16 [20]> (mapcar #'print '(a b c))

A 
B 
C 
(A B C)
Break 16 [20]> (ugraph->png "uwizard.dot" *wizard-nodes* *wizard-edges* )

*** - EVAL: la variable LIST no tiene ningún valor
Es posible continuar en los siguientes puntos:
USE-VALUE      :R1      You may input a value to be used instead of LIST.
STORE-VALUE    :R2      You may input a new value for LIST.
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
ABORT          :R17     Abort debug loop
ABORT          :R18     Abort debug loop
ABORT          :R19     Abort main loop
Break 17 [21]> (defun udges->dot (edges)
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                    (unless (assoc (car edge) (cdr lst))
                      (fresh-line)
                      (princ (dot-name (caar lst)))
                      (princ "--")
                      (princ (dot-name (car edge)))
                      (princ "[label=\"")
                      (princ (dot-label (cdr edge)))
                      (princ "\"];")))
                   (cdar lst)))
             edges))
UDGES->DOT
Break 17 [21]> (ugraph->png "uwizard.dot" *wizard-nodes* *wizard-edges* )

** - Continuable Error
OPEN: #<OUTPUT BUFFERED FILE-STREAM CHARACTER #P"uwizard.dot"> already points to file
      "/home/lfborjas/workspace/land_of_lisp/uwizard.dot"
     , opening the file again for :OUTPUT may produce unexpected results
Si continúa (tecleando `continue'): Open the file anyway
The following restarts are also available:
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
ABORT          :R16     Abort debug loop
ABORT          :R17     Abort debug loop
ABORT          :R18     Abort main loop
Break 18 [22]> (ugraph->png "uwizard.dot" *wizard-nodes* *wizard-edges* )
0
Break 18 [22]> 
0
Break 17 [21]> 
[23]> 
Adiós.
