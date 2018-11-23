#lang racket
#| 
   The Dice of Doom game, the AI version  
   ----------------------------------------

   The Dice of Doom game is a turn-based game for two players sharing one keyboard. 
   This implementation employs a lazy strategy to build the complete game 
   tree of all possible moves. If you have a large enough monitor, you can play 
   it for pretty large boards. The defauly is a 3x3 board. 

   Each player owns hexagonal territories, which are arranged into a planar game
   board. A territory comes with a number of dice. When it is a player's turn, 
   she marks one of her territories as a launching pad for an attack at a 
   neigboring territory of the other player. Such an attack is enabled only if 
   her chosen territory has more dice than the territory of the other player. 
   The effect of the attack is that the territory changes ownership and that all
   but one of the attack dice are moved to the newly conquered territory. A 
   player may continue her turn as long as she can launch attacks. Optionally, 
   she may choose to pass after her first attack is executed, meaning she ends 
   her turn. At the end of a turn, a number of dices are distributed across the 
   players' territories. The game is over when a player whose turn it is cannot
   attack on her first move. 

   A player can use the following five keys to play the game:
    -- with ← and → (arrow keys), the player changes the territory focus 
    -- with enter, the player marks a territory the launching pad for an attack
    -- with the "d" key, the player unmarks a territory 
    -- with the "p" key the player passes. 
   Once a player passes, the game announces whose turn it is next. 

   Play
   ----
 
   Run and evaluate 
     (roll-the-dice)
   This will pop up a window that the game board, and instructions. 

   If you wish to play with a larger or smaller board than the defauly, evaluate 
     (set-grid <n>)
   for the desired size <n> before you roll the dice. 

   Good luck. 
|#

(require 2htdp/image (except-in 2htdp/universe left right)) 

(define (set-grid n)
  (set! BOARD n)
  (set! GRID (* n n)))

;; Constants

; initalization constants
(define PLAYER# 2)
(define DICE# 4)
(define BOARD 5)
(define GRID (* BOARD BOARD))
(define INIT-PLAYER 0)
(define INIT-SPARE-DICE 20)
; The depth at which to limit the gametree
(define AI-DEPTH 4)
(define AI 1)

; graphical constants: territories
(define DICE-OFFSET 6)
(define SIDE 75)
(define OFFSET0 (* 2 SIDE))
(define ROTATION 30)
(define HEX 6)
(define (hexagon color)
  (rotate ROTATION (regular-polygon SIDE HEX "solid" color)))
(define X-OFFSET (image-width (hexagon "black")))
(define Y-OFFSET (* (image-height (hexagon "black")) 3/4))

; graphical constants
(define COLORS 
  (list (make-color 255 0 0 100) 
        (make-color 0 255 0 100) 
        (make-color 0 0 255 100)))
(define FOCUS (rotate ROTATION (regular-polygon SIDE 6 "outline" "black")))
(define D1 (bitmap "graphics/dice1.png"))
(define D2 (bitmap "graphics/dice2.png"))
(define D3 (bitmap "graphics/dice3.png"))
(define D4 (bitmap "graphics/dice4.png"))
(define IMG-LIST (list D1 D2 D3 D4)) 

(define TEXT-SIZE 25)
(define TEXT-COLOR "black")
(define INSTRUCT 
  "← and → to move among territories, <enter> to mark, <d> to unmark, and <p> to pass.")
(define AI-TURN "It's the Mighty AI's turn")
(define YOUR-TURN "It's your turn")
(define INFO-X-OFFSET 100)
(define INFO-Y-OFFSET 50)

(define INSTRUCTIONS (text INSTRUCT TEXT-SIZE TEXT-COLOR))
(define WIDTH (+ (image-width INSTRUCTIONS) 50))
(define HEIGHT 600)
(define (PLAIN)
  (define iw (image-width INSTRUCTIONS))
  (define bw (* SIDE 2 BOARD))
  (set! WIDTH  (+ (max iw bw) 50))
  (set! HEIGHT (+ (* SIDE 2 BOARD) 50))
  (empty-scene WIDTH HEIGHT))
(define (ISCENE)
  (define mt (PLAIN))
  ;; not sure why they have this restriction: yes, it's wider than my
  ;; screen, but not by much! I just commented it out with the # reader
  ;; macro
  #(when (or (> (image-width mt) 1280) (> (image-height mt) 800))
    (error 'scene "it is impossible to draw a ~s x ~s game scene for a 1280 x 800 laptop screen" (image-width mt) (image-height mt)))
  (place-image INSTRUCTIONS (* .5 WIDTH) (* .9 HEIGHT) mt))

;;; Data

;; src: #f or a territory chosen to launch an attach, board is all territories
;; gt is the game tree with the current state of the board as
;; a starting point
(struct dice-world (src board gt) #:transparent)

;; index: location amongst territories, and unique id
;; player: owner of the territory
;; dice: number of dice in the territory
;; x,y: coordinates to help rendering
(struct territory (index player dice x y) #:transparent)


;; we define game to be a lazy structure:
;; this way, we don't need to change everywhere in the code
;; to reify the promises that the tree-builder function makes
;; since it all was accessed by game-moves:
(define-values (game game? game-board game-player game-moves)
  (let []
    (struct game (board player delayed-moves) #:transparent)
    (values game
            game?
            game-board
            game-player
            ;; override the expected accessor with a reification
            ;; of the encapsulated moves promise
            (λ (g) (force (game-delayed-moves g))))))

;; a node is a vertex between game tree nodes
;; action: an attack to be performed, either `empty` or a list
;;         of two numbers (source, destination)
;; gt:     game tree resulting from executing the given move
(struct move (action gt) #:transparent)

;;; Behavior

(define (roll-the-dice)
  (big-bang (create-world-of-dice-and-doom)
            (on-key interact-with-board)
            (on-draw draw-dice-world)
            (stop-when no-more-moves-in-world?
                       draw-end-of-dice-world)))

(define (create-world-of-dice-and-doom)
  (define board (territory-build))
  (define gamet (game-tree board INIT-PLAYER INIT-SPARE-DICE))
  (define new-world (dice-world #f board gamet))
  (if (no-more-moves-in-world? new-world)
      ;; try again if there are no moves in the fresh world
      (create-world-of-dice-and-doom)
      new-world))

(define (no-more-moves-in-world? w)
  (define tree (dice-world-gt w))
  (define board (dice-world-board w))
  (define player (game-player tree))
  ;; are there no more moves in the world, or has
  ;; the current player conquered all territories?
  (or (no-more-moves? tree)
      (for/and ([t board])
        (= (territory-player t) player))))

(define (draw-end-of-dice-world w)
  (define board (dice-world-board w))
  (define message (text (won board) TEXT-SIZE TEXT-COLOR))
  (define background (add-board-to-scene w (PLAIN)))
  (overlay message background))

(define (draw-dice-world w)
  (add-player-info
   (game-player (dice-world-gt w))
   (add-board-to-scene w (ISCENE))))

(define (interact-with-board w k)
  (cond [(key=? "left" k)
         (refocus-board w left)]
        [(key=? "right" k)
         (refocus-board w right)]
        [(key=? "p" k)
         (pass w)]
        [(key=? "\r" k)
         (mark w)]
        [(key=? "d" k)
         (unmark w)]
        [else w]))

(define (add-player-info player s)
  (define str (whose-turn player))
  (define txt (text str TEXT-SIZE TEXT-COLOR))
  (place-image txt (- WIDTH INFO-X-OFFSET) INFO-Y-OFFSET s))

(define (whose-turn player)
  (if (= player AI) AI-TURN YOUR-TURN))

(define (add-board-to-scene w s)
  (define board  (dice-world-board w))
  (define player (game-player (dice-world-gt w)))
  (define focus? (dice-world-src w))
  (define trtry1 (first board))
  (define p-focus (territory-player trtry1))
  (define t-image (draw-territory trtry1))
  (define image (draw-focus focus? p-focus player t-image))
  (define base-s (add-territory trtry1 image s))
  (for/fold ([s base-s]) ([t (rest board)])
    (add-territory t (draw-territory t) s)))

(define (draw-focus marked? p-in-focus p t-image)
  (if (or (and (not marked?) (= p-in-focus p))
          (and marked? (not  (= p-in-focus p))))
      (overlay FOCUS t-image)
      t-image))

(define (add-territory t image scene)
  (place-image image (territory-x t) (territory-y t) scene))

(define (draw-territory t)
  (define color (color-chooser (territory-player t)))
  (overlay (hexagon color) (draw-dice (territory-dice t))))

;; choose a color based on the players identity
(define (color-chooser n)
  (list-ref COLORS n))

(define (draw-dice n)
  ;; ERRATA: the book calls it get-dice-image
  (define first-dice (get-dice-img 0))
  (define height-dice (image-height first-dice))
  (for/fold ([s first-dice])
            ([i (- n 1)])
    (define dice-image (get-dice-img (+ i 1)))
    (define y-offset (* height-dice (+ .5 (* i .25))))
    (overlay/offset s 0 y-offset dice-image)))

(define (get-dice-img i)
  (list-ref IMG-LIST
            (modulo i (length IMG-LIST))))

(define (refocus-board w direction)
  (define source (dice-world-src w))
  (define board  (dice-world-board w))
  (define tree   (dice-world-gt w))
  (define player (game-player tree))
  (define (owner? tid)
    (if source (not (= tid player)) (= tid player)))
  (define new-board (rotate-until owner? board direction))
  (dice-world source new-board tree))

(define (rotate-until owned-by board rotate)
  (define next-list (rotate board))
  (if (owned-by (territory-player (first next-list)))
      next-list
      (rotate-until owned-by next-list rotate)))

(define (left l)
  (append (rest l)
          (list (first l))))

(define (right l)
  (reverse (left (reverse l))))

(define (pass w)
  ;; passing is also a vertex in the game tree, so
  ;; we see if it's possible from this node
  (define m (find-move (game-moves (dice-world-gt w)) empty))
  (cond [(false? m) w]
        [(or (no-more-moves? m) (not (= (game-player m) AI)))
         (dice-world #f (game-board m) m)]
        [else
         (define ai (the-ai-plays m))
         (dice-world #f (game-board ai) ai)]))

(define (find-move moves action)
  (define m
    ;; findf is a "find-first"
    (findf (λ (m)
             (equal? (move-action m) action))
           moves))
  (and m (move-gt m)))

(define (mark w)
  (define tree   (dice-world-gt w))
  (define board  (dice-world-board w))
  (define source (dice-world-src w))
  (define focus  (territory-index (first board)))
  (if source
      ;; if there's a source, the next thing to mark is the dest
      (attacking w source focus)
      ;; if there's no source territory, mark the current one
      (dice-world focus board tree)))

(define (attacking w source target)
  (define feasible (game-moves (dice-world-gt w)))
  (define attack   (list source target))
  (define next     (find-move feasible attack))
  (if next
      ;; if the attack is feasible, create a new world
      ;; from the new state of the game tree
      ;; notice that the source is #f, so we're ready to mark
      ;; a new one for a new attack.
      (dice-world #f (game-board next) next)
      ;; if the attack isn't feasible, the world remains the same
      w))

;; ERRATA: this fn isn't defined in the book:
(define (no-more-moves? g)
  (empty? (game-moves g)))

(define (unmark w)
  (dice-world #f (dice-world-board w) (dice-world-gt w)))

;;; GAME TREE MAGIC

(define (territory-build)
  ;; for/list is a map, with the for exp + the exp to repeat in tail
  ;; in this case, we create territories with a random number of dice
  ;; and its position according to each ordinal in the grid
  (for/list ([n (in-range GRID)])
    (territory n
               (modulo n PLAYER#)
               (dice)
               (get-x n)
               (get-y n))))

(define (dice)
  (add1 (random DICE#)))

(define (get-x n)
  (+ OFFSET0
     (if (odd? (get-row n)) 0 (/ X-OFFSET 2))
     (* X-OFFSET (modulo n BOARD))))

(define (get-y n)
  (+ OFFSET0 (* Y-OFFSET (get-row n))))

(define (get-row pos)
  (quotient pos BOARD))

;; this is the LAZY version of game tree, notice that
;; `forcing` it will only make it calculate one more
;; level of the tree, delaying the next.
(define (game-tree board player dice)
  (define (attacks board)
    ;; a for*/list takes multiple lists and creates
    ;; embedded loops, the below, thus, is O(n**2)
    ;; explores all neighbors and gets the attackables
    ;; for all territories.
    (for*/list ([src board]
                [dst (neighbors (territory-index src))]
                #:when (attackable? board player src dst))
      (define from (territory-index src))
      (define dice (territory-dice src))
      (define newb (execute board player from dst dice))
      ;; instead of eagerly generating more moves,
      ;; we create a promise that will only be reified (forced)
      ;; when needed
      (define gt-attack
        (game newb player (delay (cons (passes newb)
                                       (attacks newb)))))
      (move (list from dst) gt-attack)))
  (define (passes board)
    ;; define-values expects multiple return values and
    ;; destructures them. Like define, it makes them available
    ;; to the enveloping lexical scope (which is why they're
    ;; available below--I got confused with the syntax of let
    ;; for a bit).
    (define-values (new-dice newb)
      (distribute board player dice))
    (move empty (game-tree newb (switch player) new-dice)))
  ;; instead of finding out the attacks at the outset,
  ;; we delay them
  (game board player (delay (attacks board))))

(define (switch player)
  ;; easy to switch when players are represented as numbers!
  (modulo (add1 player) PLAYER#))

(define (distribute board player spare-dice)
  (for/fold ([dice spare-dice]
             [new-board empty])
            ([t board])
    ;; if the given player owns a t, it has fewer than the max
    ;; dice a territory can have, and we have dice to spare still,
    ;; add one of the spare dice to that t.
    (if (and (= (territory-player t) player)
             (< (territory-dice t) DICE#)
             (not (zero? dice)))
        (values (- dice 1) (cons (add-dice-to t) new-board))
        (values dice (cons t new-board)))))

(define (add-dice-to t)
  (territory-set-dice t (add1 (territory-dice t))))


;;; THE NEXT TWO FNS ARE NOT GIVEN IN THE BOOK
;;; notice how this particular example prefers immutable data structures
;;; because mutation would screw up other alternative branches
;;; of the game tree that refer to the same territory!
;; Territory Natural -> Territory 
;; updates number of dice on territory 
(define (territory-set-dice t d)
  (territory (territory-index t)
             (territory-player t) d
             (territory-x t) (territory-y t)))

;; Territory Player -> Territory 
;; updates owner of territory 
(define (territory-set-player t p)
  (territory (territory-index t) p
             (territory-dice t)
             (territory-x t) (territory-y t)))

(define (add b x)
  (if b empty (list x)))

(define (neighbors pos)
  (define top?      (< pos BOARD))
  (define bottom?   (= (get-row pos) (sub1 BOARD)))
  (define even-row? (zero? (modulo (get-row pos) 2)))
  (define right?    (zero? (modulo (add1 pos) BOARD)))
  (define left?     (zero? (modulo pos BOARD)))
  (if even-row?
      (even-row pos top? bottom? right? left?)
      (odd-row  pos top? bottom? right? left?)))

;; by using `add` we only append the actually valid positions
(define (even-row pos top? bottom? right? left?)
  (append (add (or top? right?)    (add1 (- pos BOARD)))
          (add (or bottom? right?) (add1 (+ pos BOARD)))
          (add top?                (- pos BOARD))
          (add bottom?             (+ pos BOARD))
          (add right?              (add1 pos))
          (add left?               (sub1 pos))))


;;; NOTE: not defined in book, but they told me so.
(define (odd-row pos top? bottom? right? left?)
  (append (add top?               (- pos BOARD))
          (add bottom?            (+ pos BOARD))
          (add (or top? left?)    (sub1 (- pos BOARD)))
          (add (or bottom? left?) (sub1 (+ pos BOARD)))
          (add right?             (add1 pos))
          (add left?              (sub1 pos))))

(define (attackable? board player src dst)
  ;; check if the destination territory is actually in the board
  ;; that way we can throw simple sequences at this fn
  ;; and trust it will do the right thing.
  (define dst-t
    (findf (λ (t) (= (territory-index t) dst))
           board))
  (and dst-t
       ;; encodes the rules of the game:
       ;; can attack if it's not your territory and you
       ;; have more dice than them
       (= (territory-player src) player)
       (not (= (territory-player dst-t) player))
       (> (territory-dice src) (territory-dice dst-t))))

;; enacts an attack
(define (execute board player src dst dice)
  (for/list ([t board])
    (define idx (territory-index t))
    (cond [(= idx src) (territory-set-dice t 1)]
          [(= idx dst)
           (define s (territory-set-dice t (- dice 1)))
           (territory-set-player s player)]
          [else t])))

(define (won board)
  ;; indulging in my own neologisms here:
  (define-values [best-score w] (winners board))
  (if (cons? (rest w))
      "It's a tie!"
      "You won!"))

;; notice the casual use of multiple return values here
(define (winners board)
  (for/fold ([best 0]
             [winners empty])
            ([p PLAYER#])
    (define p-score (sum-territory board p))
    (cond [(> p-score best) (values p-score (list p))]
          [(< p-score best) (values best winners)]
          [(= p-score best) (values best (cons p winners))])))

(define (sum-territory board player)
  (for/fold ([result 0])
            ([t board])
    (if (= (territory-player t) player)
        (+ result 1)
        result)))


;; Add AI: uses minimax to evaluate up to 4 (AI-DEPTH) levels ahead

(define (rate-moves tree depth)
  (for/list ([move (game-moves tree)])
    (list move (rate-position (move-gt move) (- depth 1)))))

;; give a weight to a given branch: if we're out of depth/moves
;; and not a winner, this is a worthless branch. Else, we
;; need to evaluate further moves, choosing either the best
;; for the AI or the worst for the other player.
(define (rate-position tree depth)
  (cond [(or (= depth 0) (no-more-moves? tree))
         (define-values (best w) (winners (game-board tree)))
         (if (member AI w) (/ 1 (length w)) 0)]
        [else
         (define ratings (rate-moves tree depth))
         (apply (if (= (game-player tree) AI) max min)
                (map second ratings))]))

(define (the-ai-plays tree)
  (define ratings (rate-moves tree AI-DEPTH))
  ;; argmax finds the rating with the highest rating (second element
  ;; of a rate-moves list, with the first being the move itself).
  (define the-move (first (argmax second ratings)))
  (define new-tree (move-gt the-move))
  (if (= (game-player new-tree) AI)
      (the-ai-plays new-tree)
      new-tree))

;; To play, uncomment these or run from the REPL
#(set-grid 4)
#(roll-the-dice)
