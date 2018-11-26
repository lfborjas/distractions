;;; Chapter 19: a Web UI for Dice of Doom

;;; Requires code from previous chapters:
;;; (load "dice_of_doom_v2.lisp")
;;; (load "webserver.lisp")
;;; (load "svg.lisp")

;;; CONSTANTS

(defparameter *board-width*  900)
(defparameter *board-height* 500)
(defparameter *board-scale*  64)
(defparameter *top-offset*   3)
(defparameter *dice-scale*   40)
(defparameter *dot-size*     0.05)
(defparameter *die-colors*   '((255 63 63)
                               (63 63 255)
                               (63 255 63)))

;;; RENDERING

;; generate an SVG to represent a 3D die on a given starting point
;; in the canvas and a color.
(defun draw-die-svg (x y col)
  ;; calc-pt scales all points to the configured scale
  (labels ((calc-pt (pt)
             (cons (+ x (* *dice-scale* (car pt)))
                   (+ y (* *dice-scale* (cdr pt)))))
           (f (pol col)
             (polygon (mapcar #'calc-pt pol) col)))
    ;; draw each side (only three visible sides)
    (f '((0 . -1) (-0.6 . -0.75) (0 . -0.5) (0.6 . -0.75))
       (brightness col 40))
    (f '((0 . -0.5) (-0.6 . -0.75) (-0.6 . 0) (0 . 0.25))
       col)
    (f '((0 . -0.5) (0.6 . -0.75) (0.6 . 0) (0 . 0.25))
       (brightness col -40))
    ;; draw the points: mapc applies a fn to elements of lists
    ;; with the same index (in this case, in pairs)
    (mapc (lambda (x y)
            (polygon (mapcar (lambda (xx yy)
                               (calc-pt (cons (+ x (* xx *dot-size*))
                                              (+ y (* yy *dot-size*)))))
                             '(-1 -1 1  1)
                             '(-1  1 1 -1))
                     ;; dots are white, no need for diff colors like
                     ;; die have.
                     '(255 255 255)))
          '(-0.05 0.125 0.3 -0.3 -0.125 0.05 0.2 0.2 0.45 0.45 -0.45 -0.2)
          '(-0.875 -0.80 -0.725 -0.775 -0.70 -0.625
            -0.35 -0.05 -0.45 -0.15 -0.45 -0.05))))

;; Here's what a dice looks like (open sample_dice.svg on a web browser)
;; (with-open-file (*standard-output* "sample_dice.svg"
;;                                    :direction :output
;;                                    :if-exists :supersede)
;;   (svg 100 100 (draw-die-svg 50 50 '(255 0 0))))

;; Draw a tile (with its dice)
(defun draw-tile-svg (x y pos hex xx yy col chosen-tile)
  ;; make the base look like a 3d piece by drawing it twice,
  ;; one segment on top of the other.
  (loop for z below 2
        do (polygon (mapcar (lambda (pt)
                              (cons (+ xx (* *board-scale* (car pt)))
                                    (+ yy (* *board-scale*
                                             (+ (cdr pt) (* (- 1 z) 0.1))))))
                            ;; six points to a hexagon
                            '((-1 . -0.2) (0 . -0.5) (1 . -0.2)
                              (1 . 0.2)   (0 . 0.5)  (-1 . 0.2)))
                    ;; highlight if this is the chosen tile
                    (if (eql pos chosen-tile)
                        (brightness col 100)
                        col)))
  ;; draw the dice:
  (loop for z below (second hex)
        do (draw-die-svg (+ xx
                            (* *dice-scale*
                               0.3
                               ;; shift the odd tiles a bit to the left
                               ;; so they look more natural/imperfect.
                               (if (oddp (+ x y z))
                                   -0.3
                                   0.3)))
                         (- yy (* *dice-scale* z 0.8)) col)))

;; Here's how a hexagon with its dice would look like
;; (with-open-file (*standard-output* "sample_tile.svg"
;;                                    :direction :output
;;                                    :if-exists :supersede)
;;   (svg 300 300 (draw-tile-svg 0 0 0 '(0 3) 100 150 '(255 0 0) nil)))

;; Draw the entire board as an SVG
(defun draw-board-svg (board chosen-tile legal-tiles)
  (loop for y below *board-size*
        ;; we're iterating on all these values at the same time (not nested)
        do (loop for x below *board-size*
                 for pos = (+ x (* *board-size* y))
                 for hex = (aref board pos)
                 ;; we're drawing the board in perspective, hence the
                 ;; manipulated coords.
                 for xx  = (* *board-scale* (+ (* 2 x) (- *board-size* y)))
                 for yy  = (* *board-scale* (+ (* y 0.7) *top-offset*))
                 ;; darken depending on the y coordinate to give
                 ;; the impression of 3D distance.
                 for col = (brightness (nth (first hex) *die-colors*)
                                       (* -15 (- *board-size* y)))
                 ;; highlight the chosen tile and the clickable tiles
                 do (if (or (member pos legal-tiles) (eql pos chosen-tile))
                        (tag g ()
                          (tag a ("xlink:href" (make-game-link pos))
                            (draw-tile-svg x y pos hex xx yy col chosen-tile)))
                        (draw-tile-svg x y pos hex xx yy col chosen-tile)))))

(defun make-game-link (pos)
  (format nil "/game.html?chosen=~a" pos))

;; Here's how a full, 3D, in perspective, board looks like:
;; (with-open-file (*standard-output* "sample_board.svg"
;;                                    :direction :output
;;                                    :if-exists :supersede)
;;   (svg *board-width* *board-height* (draw-board-svg (gen-board) nil nil)))

;;; WEB SERVER

(defparameter *cur-game-tree* nil)
(defparameter *from-tile* nil)

;; Simple request handler for the game; notice that it only maintains
;; state for one human player (cur-game-tree, from-tile). It should
;; be possible to add a hash table of trees for multiple players.
(defun dod-request-handler (path header params)
  ;; ERRATA: Added these headers since all responses need them,
  ;; the book seems to have been using some old school browser
  ;; that was fine with no headers. None are fine with that now.
  (princ "HTTP/1.1 200 OK")
  (terpri)
  (princ "Content-Type: text/html")
  (terpri) ;; using terpri vs. fresh-line to avoid it skipping the next, blank, line
  (princ "") ;; mandatory blank line between headers and body
  (terpri)
  ;; this is where the book begins this fn:
  (if (equal path "game.html")
      (progn (princ "<!doctype html")
             (tag center ()
               (princ "Welcome to DICE OF DOOM!")
               (tag br ())
               (let ((chosen (assoc 'chosen params)))
                 (when (or (not *cur-game-tree*) (not chosen))
                   (setf chosen nil)
                   (web-initialize))
                 (cond ((lazy-null (caddr *cur-game-tree*))
                        (web-announce-winner (cadr *cur-game-tree*)))
                       ((zerop (car *cur-game-tree*))
                        (web-handle-human
                         (when chosen
                           (read-from-string (cdr chosen)))))
                       (t (web-handle-computer))))
               (tag br ())
               (draw-dod-page *cur-game-tree* *from-tile*)))
      (princ "Sorry, I don't know that page.")))

(defun web-initialize ()
  (setf *from-tile* nil)
  (setf *cur-game-tree* (game-tree (gen-board) 0 0 t)))

(defun web-announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
        (format t "The winner is ~a" (player-letter (car w)))))
  (tag a (href "game.html")
    (princ " play again")))

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
        (t (setf *cur-game-tree*
                 (cadr (lazy-find-if (lambda (move)
                                       (equal (car move)
                                              (list *from-tile* pos)))
                                     (caddr *cur-game-tree*))))
           (setf *from-tile* nil)
           (princ "You may now ")
           (tag a (href (make-game-link 'pass))
             (princ "pass"))
           (princ " or make another move:"))))

(defun web-handle-computer ()
  (setf *cur-game-tree* (handle-computer *cur-game-tree*))
  (princ "The computer has moved. ")
  (tag script ()
    (princ
     ;; simulate an "animation" of what the computer did
     "window.setTimeout('window.location=\"game.html?chosen=NIL\"', 5000)")))

(defun draw-dod-page (tree selected-tile)
  (svg *board-width* *board-height*
    (draw-board-svg (cadr tree)
                    selected-tile
                    (take-all (if selected-tile
                                  (lazy-mapcar
                                   (lambda (move)
                                     (when (eql (caar move)
                                                selected-tile)
                                       (cadar move)))
                                   (caddr tree))
                                  (lazy-mapcar #'caar (caddr tree)))))))

;; To play the game, simply run:
; (serve #'dod-request-handler)
