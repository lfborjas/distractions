;;; Chapter 15 of the book: Dice of Doom simplified, text-based.

(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 3) ;; without memoization and lazy eval, 3 is too much.
(defparameter *board-hexnum* (* *board-size* *board-size*))

;; be able to represent the board as an array, not a list,
;; for fast lookups (constant-time for arrays, linear for lists)
(defun board-array (lst)
  (make-array *board-hexnum*
              :initial-contents lst))

;; generate a random initial board, as an array:
;; CL-USER> (gen-board)
;; #((0 1) (0 3) (1 3) (1 2))
(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
                     collect (list (random *num-players*)
                                   (1+ (random *max-dice*))))))

;; players are represented as ints, give them a friendlier
;; display name:
;; CL-USER> (player-letter 0)
;; #\a
;; CL-USER> (player-letter 1)
;; #\b
(defun player-letter (n)
  (code-char (+ 97 n)))

;; Draw board with the players and their number of dice,
;; with the "tilted" look of our future hexagonal board.
;; CL-USER> (draw-board (gen-board))
;;   a-3 b-1 
;;  b-1 a-1 
(defun draw-board (board)
  (loop for y below *board-size*
        do (progn (fresh-line)
                  (loop repeat (- *board-size* y)
                        do (princ " "))
                  (loop for x below *board-size*
                        for hex = (aref board (+ x (* *board-size* y)))
                        do (format t "~a-~a "
                                   (player-letter (first hex))
                                   (second hex))))))

;;; FUNCTION PIPELINE: encode game rules in a lazy
;;; game tree that all player modalities can use.
;;; this approach is described in:
;;; https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf

;; master function: generate the game tree,
;; meant to be called only once, the game logic simply
;; traverses this tree to make sure moves conform to possible moves
;; as represented by vertices between different node/states of the game.
;; Since this is an eager version, it'll be abusively slow in large trees;
;; here's an example of a small one:
;; CL-USER> (game-tree #((0 1) (1 1) (0 2) (1 1)) 0 0 t)
;; (0 #((0 1) (1 1) (0 2) (1 1))
;;  (((2 3)
;;    (0 #((0 1) (1 1) (0 1) (0 1)) ((NIL (1 #((0 1) (1 1) (0 1) (0 1)) NIL)))))))
(defun game-tree (board player spare-dice first-move)
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      ;; passing isn't permitted on the first move, so we just add it
      ;; if it isn't the first move.
      (cons (list nil
                  (game-tree (add-new-dice board player (1- spare-dice))
                             (mod (1+ player) *num-players*)
                             0
                             t))
            moves)))

;; Find all possible legal attacking moves for a given player,
;; by visiting all hexagons and considering all neighbors
;; of each that aren't owned by the player and have fewer dice.
(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
             (car (aref board pos)))
           (dice (pos)
             (cadr (aref board pos))))
    ;; mapcan expects each iteration to return one or many
    ;; lists, the final list will be a concatenation of all.
    ;; Visit all the hexagons:
    (mapcan (lambda (src)
              (when (eq (player src) cur-player)
                ;; for each hexagon, visit all its neighbors.
                (mapcan (lambda (dst)
                          (when (and (not (eq (player dst) cur-player))
                                     (> (dice src) (dice dst)))
                            (list
                             (list (list src dst)
                                   (game-tree (board-attack board cur-player src dst (dice src))
                                              cur-player
                                              (+ spare-dice (dice dst))
                                              nil)))))
                        (neighbors src))))
            (loop for n below *board-hexnum*
                  collect n))))

;; Given a position, find all possible neighbors; gotta do
;; some bound checks to avoid the edges.
(defun neighbors (pos)
  (let ((up   (- pos *board-size*))
        (down (+ pos *board-size*)))
    (loop for p in (append (list up down)
                           (unless (zerop (mod pos *board-size*))
                             (list (1- up)  (1- pos)))
                           (unless (zerop (mod (1+ pos) *board-size*))
                             (list (1+ pos) (1+ down))))
          when (and (>= p 0) (< p *board-hexnum*))
            collect p)))

;; figures out what happens when we attack:
;; if it's the source, we now only have one dice in there (as per the rules)
;; and if it's the destination, we have all our previous dice of the src - 1
(defun board-attack (board player src dst dice)
  (board-array (loop for pos from 0
                     for hex across board
                     collect (cond ((eq pos src) (list player 1))
                                   ((eq pos dst) (list player (1- dice)))
                                   (t hex)))))

;; add reinforcemts to a given players dice, depending on
;; how many were captured in the last turn:
;; notice for example how it only adds more dice to squares
;; owned by player 0:
;; CL-USER> (add-new-dice #((0 1) (1 3) (0 2) (1 1)) 0 2)
;; #((0 2) (1 3) (0 3) (1 1))

;; This is the non-TCO version of add-new-dice: notice that it recurs
;; on `f` in a non-tail-call (needs to keep the stack to be able to do the cons
;; calls). 
;; (defun add-new-dice (board player spare-dice)
;;   (labels ((f (lst n)
;;              (cond ((null lst) nil)
;;                    ((zerop n)  lst)
;;                    (t (let ((cur-player (caar lst))
;;                             (cur-dice   (cadar lst)))
;;                         (if (and (eq cur-player player) (< cur-dice *max-dice*))
;;                             (cons (list cur-player (1+ cur-dice))
;;                                   (f (cdr lst) (1- n)))
;;                             (cons (car lst) (f (cdr lst) n))))))))
;;     (board-array (f (coerce board 'list) spare-dice))))

;; this is the tail-call optimizable version of add-new-dice:
;; it uses the "accumulator" technique to be able to discard the stack
(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n acc)
             (cond ((zerop n)  (append (reverse acc) lst))
                   ((null lst) (reverse acc))
                   (t (let ((cur-player (caar lst))
                            (cur-dice   (cadar lst)))
                        (if (and (eq cur-player player)
                                 (< cur-dice *max-dice*))
                            (f (cdr lst)
                               (1- n)
                               (cons (list cur-player (1+ cur-dice)) acc))
                            (f (cdr lst) n (cons (car lst) acc))))))))
    (board-array (f (coerce board 'list) spare-dice ()))))

;; To ensure TCO in clisp, one must compile the fn:
;; (compile 'add-new-dice)
;; CL-USER> (compile 'add-new-dice)
;; ADD-NEW-DICE

;;; At this point, we've defined all the helper fns required by game-tree.

;;; HUMAN INTERACTION FNS:
(defun play-vs-human (tree)
  (print-info tree)
  (if (caddr tree)
      ;; only keep prompting humans if more moves are available
      (play-vs-human (handle-human tree))
      (announce-winner (cadr tree))))

(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))

(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (loop for move in moves
          for n from 1
          do (let ((action (car move)))
               (fresh-line)
               (format t "~a. " n)
               (if action
                   (format t "~a -> ~a" (car action) (cadr action))
                   (princ "end turn"))))
    (fresh-line)
    (cadr (nth (1- (read)) moves))))

(defun winners (board)
  (let* ((tally (loop for hex across board
                      collect (car hex)))
         (totals (mapcar (lambda (player)
                           (cons player (count player tally)))
                         (remove-duplicates tally)))
         (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'car
            (remove-if (lambda (x)
                         (not (eq (cdr x) best)))
                       totals))))

(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
        (format t "The winner is ~a" (player-letter (car w))))))


;;; AI strategies

;; Rates positions given the current state of the tree
;; for the current player (the computer)
;;
;; If there's any possible moves and it's a move-state
;; for the computer, get the vertex that has the most value
;; if not, the one with the least (minimax: "what's bad for my enemy
;; is good for me").
;; If there's no moves (end-state), rate between 0 (player's not a winner)
;; and 1 (player's the sole winner), with ties inbetween (if there's multiple
;; players)
(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if moves
        (apply (if (eq (car tree) player)
                   #'max
                   #'min)
               (get-ratings tree player))
        (let ((w (winners (cadr tree))))
          (if (member player w)
              (/ 1 (length w))
              0)))))

;; Helper fn: get all the ratings for each following move
(defun get-ratings (tree player)
  (mapcar (lambda (move)
                 (rate-position (cadr move) player))
          (caddr tree)))

;;; AI interaction

;; The computer simply picks the move with the highest rating
(defun handle-computer (tree)
  (let ((ratings (get-ratings tree (car tree))))
    (cadr (nth (position (apply #'max ratings) ratings)
               (caddr tree)))))

;; Main UI handler:
;; If there's no moves, announce the winner
;; If it's the first player's turn, we know by convention it's the human
;; else, it's the computer's turn (notice that this assumes only those two players).
(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree))  (play-vs-computer (handle-human tree)))
        (t                   (play-vs-computer (handle-computer tree)))))



;;; OPTIMIZATIONS

;; Notice how we use a let-over-lambda closure to
;; memoize using a hash-table, and symbol-function
;; to retrieve our previous implementation of the fn;
;; not only is this dynamic, it feels like a "decorator".
;; Also, notice how `setf` can take a getter and use it
;; to set too; I think that's a pretty cool CL idiom.
(let ((old-neighbors (symbol-function 'neighbors))
      (previous      (make-hash-table)))
  (defun neighbors (pos)
    (or (gethash pos previous)
        (setf (gethash pos previous)
              (funcall old-neighbors pos)))))

;; Notice how we use a different equality test (default is `eql`)
;; to make sure the game tree array is deep-compared before
;; deciding it's in the hash table. Also, using &rest
;; is a neat trick to not have to type out all the original parameters,
;; since we don't really need them in this context.
(let ((old-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test #'equalp)))
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
        (setf (gethash rest previous)
              (apply old-game-tree rest)))))

;; Notice how we have embedded hash tables, for trees and players,
;; to not have to apply something slow like `equal` to the
;; potentially large `tree`
(let ((old-rate-position (symbol-function 'rate-position))
      (previous (make-hash-table)))
  (defun rate-position (tree player)
    (let ((tab (gethash player previous)))
      (unless tab
        (setf tab (setf (gethash player previous) (make-hash-table))))
      (or (gethash tree tab)
          (setf (gethash tree tab)
                (funcall old-rate-position tree player))))))

;; After the optimizations, you'll notice that the computer takes a few seconds
;; computing the initial tree and its initial turn; afterwards, it's pretty
;; damn fast. This is on account of the memoizations and the TCO.
;; When we introduce lazy evaluation later on, we'll also see less start-up lag.

;; Example Human v Human game (notice that sometimes it generates games with no
;; possible moves and just announces a winner instantly:

;; CL-USER> (play-vs-human (game-tree (gen-board) 0 0 t))
;; current player = a
;;   a-2 a-3 
;;  a-3 b-3 
;; The winner is a
;; NIL
;; CL-USER> (play-vs-human (game-tree (gen-board) 0 0 t))
;; current player = a
;;   b-1 b-2 
;;  a-1 a-3 
;; choose your move:
;; 1. 3 -> 1
;; 2. 3 -> 0
;; 2
;; current player = a
;;   a-2 b-2 
;;  a-1 a-1 
;; choose your move:
;; 1. end turn
;; 1
;; current player = b
;;   a-2 b-2 
;;  a-1 a-1 
;; choose your move:
;; 1. 1 -> 3
;; 1
;; current player = b
;;   a-2 b-1 
;;  a-1 b-1 
;; choose your move:
;; 1. end turn
;; 1
;; current player = a
;;   a-2 b-1 
;;  a-1 b-1 
;; choose your move:
;; 1. 0 -> 1
;; 2. 0 -> 3
;; 1
;; current player = a
;;   a-1 a-1 
;;  a-1 b-1 
;; choose your move:
;; 1. end turn
;; 1
;; current player = b
;;   a-1 a-1 
;;  a-1 b-1 
;; The winner is a
;; NIL

;; Example Human vs Computer game:

;; CL-USER> (play-vs-computer (game-tree (gen-board) 0 0 t))
;; current player = a
;;   a-2 b-3 
;;  b-1 a-3 
;; choose your move:
;; 1. 0 -> 2
;; 2. 3 -> 2
;; 1
;; current player = a
;;   a-1 b-3 
;;  a-1 a-3 
;; choose your move:
;; 1. end turn
;; 1
;; current player = b
;;   a-1 b-3 
;;  a-1 a-3 
;; current player = b
;;   b-2 b-1 
;;  a-1 a-3 
;; current player = a
;;   b-2 b-1 
;;  a-1 a-3 
;; choose your move:
;; 1. 3 -> 1
;; 2. 3 -> 0
;; 2
;; current player = a
;;   a-2 b-1 
;;  a-1 a-1 
;; choose your move:
;; 1. end turn
;; 2. 0 -> 1
;; 1
;; current player = b
;;   a-3 b-1 
;;  a-1 a-1 
;; The winner is a
;; NIL
