;; Dice of Doom improved: lazy evaluation, better AI. From chapter 18

;; (load "dice_of_doom_v1.lisp")
;; (load "lazy.lisp")

;; after the improvements here, we'll be able to play on a bigger board
(defparameter *board-size* 4)
(defparameter *board-hexnum* (* *board-size* *board-size*))

;; Improved version of add-passing-move, using lazy sequences to avoid
;; exploring the entire tree
(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (lazy-cons (list nil
                       (game-tree (add-new-dice board player (1- spare-dice))
                                  (mod (1+ player) *num-players*)
                                  0
                                  t))
                 moves)))

;; Improved version of attacking-moves: uses lazy seqs and its functions
(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
             (car (aref board pos)))
           (dice (pos)
             (cadr (aref board pos))))
    (lazy-mapcan
     (lambda (src)
       (if (eq (player src) cur-player)
           (lazy-mapcan
            (lambda (dst)
              (if (and (not (eq (player dst) cur-player))
                       (> (dice src) (dice dst)))
                  (make-lazy
                   (list (list (list src dst)
                               (game-tree (board-attack board
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

;; Lazy-aware versions of the human interaction functions
(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (labels ((print-moves (moves n)
               (unless (lazy-null moves)
                 (let* ((move (lazy-car moves))
                        (action (car move)))
                   (fresh-line)
                   (format t "~a. " n)
                   (if action
                       (format t "~a -> ~a" (car action) (cadr action))
                       (princ "end turn")))
                 (print-moves (lazy-cdr moves) (1+ n)))))
      (print-moves moves 1))
    (fresh-line)
    (cadr (lazy-nth (1- (read)) moves))))

(defun play-vs-human (tree)
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
      (play-vs-human (handle-human tree))
      (announce-winner (cadr tree))))

;; After these improvements, there's no noticeable delay when starting up the game
;; or choosing a move, due to the entire game tree never being fully traversed!

;;; AI IMPROVEMENTS

;; prune the AI's tree: only explore a specific depth of the tree
(defun limit-tree-depth (tree depth)
  (list (car tree)
        (cadr tree)
        (if (zerop depth)
            (lazy-nil)
            (lazy-mapcar (lambda (move)
                           (list (car move)
                                 (limit-tree-depth (cadr move)
                                                   (1- depth))))
                         (caddr tree)))))

;; Now the AI will only look 4 levels of the tree ahead, which means
;; it won't choose the _absolutely_ best possible outcome, but something
;; that looks good four moves from now. The cost of performance
;; is the AI no longer playing perfect games.
(defparameter *ai-level* 4)
(defun handle-computer (tree)
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*)
                              (car tree))))
    (cadr (lazy-nth (position (apply #'max ratings) ratings)
                    (caddr tree)))))

(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((lazy-null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))

;;; HEURISTICS:
;;; Since the AI can no longer see all the way into the leaves of the game tree
;;; we need to apply some approximations: in this case, it's more valuable to
;;; protect threatened territories, so we encode that with `score-board`

(defun score-board (board player)
  (loop for hex across board
        for pos from 0
        sum (if (eq (car hex) player)
                (if (threatened pos board)
                    1
                    2)
                -1)))

(defun threatened (pos board)
  (let* ((hex    (aref board pos))
         (player (car hex))
         (dice   (cadr hex)))
    (loop for n in (neighbors pos)
          do (let* ((nhex    (aref board n))
                    (nplayer (car  nhex))
                    (ndice   (cadr nhex)))
               (when (and (not (eq player nplayer)) (> ndice dice))
                 ;; return prematurely if we've found a threatening
                 ;; neighbor: no need to explore further
                 (return t))))))

;;; redefine get ratings and rate position to use lazy eval and heuristics:

(defun get-ratings (tree player)
  (take-all (lazy-mapcar (lambda (move)
                           (rate-position (cadr move) player))
                         (caddr tree))))

(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
        (apply (if (eq (car tree) player)
                   #'max
                   #'min)
               (get-ratings tree player))
        (score-board (cadr tree) player))))

