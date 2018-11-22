#lang racket

#| 
   The Dice of Doom game, the eager version  
   ----------------------------------------

   The Dice of Doom game is a turn-based game for two players sharing one keyboard. 
   Since this implementation employs an eager strategy to build the complete game 
   tree of all possible moves, it is only a step in the right direction. 

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
|#

(require 2htdp/image (except-in 2htdp/universe left right)) 

;; Constants

; initalization constants
(define PLAYER# 2)
(define DICE# 3)
(define BOARD 2)
(define GRID (* BOARD BOARD))
(define INIT-PLAYER 0)
(define INIT-SPARE-DICE 10)
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
  "← and → to move among territories, <enter> to mark, <d> to unmark, and <p> to pass. First you mark destination, then target.")
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
  (when (or (> (image-width mt) 1280) (> (image-height mt) 800))
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

;; a game is a node in the game tree
;; board: state of the board, i.e. list of territories
;; player: num id of the current player
;; moves: all possible moves at this state
(struct game (board player moves) #:transparent)


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
