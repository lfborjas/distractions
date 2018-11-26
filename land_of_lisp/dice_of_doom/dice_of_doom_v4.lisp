;; Chapter 20 of the book: funnest version of DoD

(defparameter *num-players* 4)
(defparameter *die-colors*   '((255 63 63) ; red
                               (63 63 255) ; blue
                               (63 255 63) ; green
                               (255 63 255) ;purple
                               ))
(defparameter *max-dice* 5)
(defparameter *ai-level* 2) ;; dumb it down a bit since there'll be more heuristics

;; Adding chance nodes to the tree: now attacking doesn't automatically
;; succeed if the attacker has more dice: the dice in each hex are rolled and
;; the attacker _only_ succeeds if their sum is greater, loses in ties.
(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
             (car (aref board pos)))
           (dice (pos)
             (cadr (aref board pos))))
    (lazy-mapcan (lambda (src)
                   (if (eq (player src) cur-player)
                       (lazy-mapcan
                        (lambda (dst)
                          (if (and (not (eq (player dst) cur-player))
                                   (> (dice src) 1))
                              (make-lazy
                               (list
                                (list
                                 (list src dst)
                                 (game-tree (board-attack board
                                                          cur-player
                                                          src
                                                          dst
                                                          (dice src))
                                            cur-player
                                            (+ spare-dice (dice dst))
                                            nil)
                                 ;; this is the only new thing going on in this
                                 ;; version of attacking-moves, the alternate branch
                                 (game-tree (board-attack-fail board
                                                               cur-player
                                                               src
                                                               dst
                                                               (dice src))
                                            cur-player
                                            (+ spare-dice (dice dst))
                                            nil))))
                              (lazy-nil)))
                        (make-lazy (neighbors src)))
                       (lazy-nil)))
                 (make-lazy (loop for n below *board-hexnum*
                                  collect n)))))

;; return everything unmodified except for the src hex, in which
;; the player loses everything but one die.
(defun board-attack-fail (board player src dst dice)
  (board-array (loop for pos from 0
                     for hex across board
                     collect (if (eq pos src)
                                 (list player 1)
                                 hex))))

;; roll the dice dice-num times and return the total
(defun roll-dice (dice-num)
  (let ((total (loop repeat dice-num
                     sum (1+ (random 6)))))
    (fresh-line)
    (format t "On ~a dice rolled ~a. " dice-num total)
    total))

(defun roll-against (src-dice dst-dice)
  (> (roll-dice src-dice)
     (roll-dice dst-dice)))

(defun pick-chance-branch (board move)
  (labels ((dice (pos)
             (cadr (aref board pos))))
    (let ((path (car move)))
      (if (or (null path) (roll-against (dice (car path))
                                        (dice (cadr path))))
          (cadr move)
          (caddr move)))))

;; Update the interaction functions to use pick-chance-branch

(defun web-handle-human (pos)
  (cond ((not pos) (princ "Please choose a hexagon to move from:"))
        ((eq pos 'pass) (setf *cur-game-tree*
                              (cadr (lazy-car (caddr *cur-game-tree*))))
         (princ "Your reinforcements have been placed.")
         (tag a (href (make-game-link nil))
           (princ "continue")))
        ;; if they hadn't chosen a src, they're choosing it now
        ((not *from-tile*) (setf *from-tile* pos)
         (princ "Now choose a destination:"))
        ;; if a player selects what they just selected,
        ;; they must be changing their mind about the src hex
        ((eq pos *from-tile*) (setf *from-tile* nil)
         (princ "Move canceled."))
        ;; since the player can only select valid moves,
        ;; anything else means we can advance the game
        ;; but in v4, we leave that to chance:
        (t (setf *cur-game-tree*
                 (pick-chance-branch
                  (cadr *cur-game-tree*)
                  (lazy-find-if (lambda (move)
                                  (equal (car move)
                                         (list *from-tile* pos)))
                                (caddr *cur-game-tree*))))
           (setf *from-tile* nil)
           (princ "You may now ")
           (tag a (href (make-game-link 'pass))
             (princ "pass"))
           (princ " or make another move:"))))

(defun handle-computer (tree)
  ;; notice we don't have alpha beta pruning here, as the presence of
  ;; chance nodes makes it way more difficult to code.
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*) (car tree))))
    (pick-chance-branch
     (cadr tree)
     (lazy-nth (position (apply #'max ratings) ratings)
               (caddr tree)))))

;; Update the AI to understand the presence of chance nodes

;; matrix of odds: rows are destination starting with one die,
;; cols are src starting with two (min to be able to attack), e.g.:
;; two attacking dice vs. one defending: 84% chance of winning
;; four against three: 74%
(defparameter *dice-odds* #(#(0.84 0.97 1.0 1.0)
                            #(0.44 0.78 0.94 0.99)
                            #(0.15 0.45 0.74 0.91)
                            #(0.04 0.19 0.46 0.72)
                            #(0.01 0.06 0.22 0.46)))

;; FULL DISCLOSURE: I didn't type up these last fns as it was late
;; and my fiancée was asleep next to me.
;; copied from: http://landoflisp.com/dice_of_doom_v4.lisp
(defun get-ratings (tree player)
  (let ((board (cadr tree)))
    (labels ((dice (pos)
                   (cadr (aref board pos))))
      (take-all (lazy-mapcar 
                  (lambda (move)
                    (let ((path (car move)))
                      (if path
                          (let* ((src (car path))
                                 (dst (cadr path))
                                 (odds (aref (aref *dice-odds* 
                                                   (1- (dice dst)))
                                             (- (dice src) 2))))
                            (+ (* odds (rate-position (cadr move) player))
                               (* (- 1 odds) (rate-position (caddr move)
                                                            player))))
                        (rate-position (cadr move) player))))
                  (caddr tree))))))

(defun limit-tree-depth (tree depth)
  (list (car tree) 
	  (cadr tree) 
	  (if (zerop depth)
	      (lazy-nil)
	    (lazy-mapcar (lambda (move)
                         (cons (car move)
                               (mapcar (lambda (x)
                                         (limit-tree-depth x (1- depth)))
                                       (cdr move))))
		           (caddr tree)))))

;; Make reinforcements more interesting: instead of the reinforcements
;; simply equaling the number of dice captured in each turn, it'll be
;; the number of tiles in the players contiguous territory. This is
;; only possible now that our tree can be "infinite".

;; note the casual mutual recursion.
(defun get-connected (board player pos)
  (labels ((check-pos (pos visited)
             (if (and (eq (car (aref board pos)) player)
                      (not (member pos visited)))
                 (check-neighbors (neighbors pos) (cons pos visited))
               visited))
           (check-neighbors (lst visited)
             (if lst
                 (check-neighbors (cdr lst) (check-pos (car lst) visited))
               visited)))
    (check-pos pos '())))


(defun largest-cluster-size (board player)
  (labels ((f (pos visited best)
	      (if (< pos *board-hexnum*)
		  (if (and (eq (car (aref board pos)) player)
                       (not (member pos visited)))
		      (let* ((cluster (get-connected board player pos))
			     (size (length cluster)))
			(if (> size best)
			    (f (1+ pos) (append cluster visited) size)
			  (f (1+ pos) (append cluster visited) best)))
		    (f (1+ pos) visited best))
		best)))
	  (f 0 '() 0)))

(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n)
	      (cond ((zerop n) lst)
		    ((null lst) nil)
		    (t (let ((cur-player (caar lst))
			     (cur-dice (cadar lst)))
			 (if (and (eq cur-player player) (< cur-dice *max-dice*))
                       (cons (list cur-player (1+ cur-dice))
                             (f (cdr lst) (1- n)))
			   (cons (car lst) (f (cdr lst) n))))))))
	  (board-array (f (coerce board 'list) 
                        (largest-cluster-size board player)))))

