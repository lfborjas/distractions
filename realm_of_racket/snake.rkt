#lang racket

;;; Chapter 6 of Realm of Racket: the guess my number UI
;;; Full source from authors available at:
;;; "/Applications/Racket v7.1/share/pkgs/realm/chapter6/source.rkt"
;;; Notice that I copied over the graphics locally.

(require 2htdp/universe 2htdp/image)

;; Constants

;; Tick Rate 
(define TICK-RATE 1/10)

;; Board Size Constants
(define SIZE 30)

;; Snake Constants
(define SEG-SIZE 15)

;; Goo Constants
(define MAX-GOO 5)
(define EXPIRATION-TIME 150)

;; GRAPHICAL BOARD
(define WIDTH-PX  (* SEG-SIZE 30))
(define HEIGHT-PX (* SEG-SIZE 30))

;; Visual constants
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define GOO-IMG (bitmap "graphics/goo.gif"))
(define SEG-IMG  (bitmap "graphics/body.gif"))
(define HEAD-IMG (bitmap "graphics/head.gif"))

(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

(define ENDGAME-TEXT-SIZE 15)

;; Data Definitions
;; The book assumes all structs are transparent, which makes them friendlier for REPL exploration

;; Main world representation: one snake, many goos
(struct pit (snake goos) #:transparent)
;; A snake has one of four "dirs" (up, down, left, right) and
;; a list of points representing its segments
(struct snake (dir segs) #:transparent)
;; posn = a position in the canvas
(struct posn (x y) #:transparent)
;; a goo has a posn location, and a number of clock ticks before
;; it disappears
(struct goo (loc expire) #:transparent)

;; Lol @ "If you had bought a serious book on programming, it would explain these functions in gory detail. But this is a cheap book, and you get what you pay for."
(define (snake-head s)
  (first (snake-segs s)))

(define (snake-body s)
  (rest (snake-segs s)))

(define (snake-tail s)
  (last (snake-segs s)))

(define (snake-change-dir s d)
  (snake d (snake-segs s)))


;; Main function
(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (list (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)))
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))

;; CLOCK TICK HANDLERS: pit, snake and goo behavior.
(define (next-pit w)
  (define snake (pit-snake w))
  (define goos (pit-goos w))
  (define goo-to-eat (can-eat snake goos))
  (if goo-to-eat
      (pit (grow snake) (age-goo (eat goos goo-to-eat)))
      (pit (slither snake) (age-goo goos))))

;; Notice how racketeers use `cond` to organize recursion, and how functions
;; of this type are called `list-eaters`.
;; Notice how it returns a boolean when there's no goo to eat, but a goo
;; if there is; a common pattern for recursive predicates in Racket.
(define (can-eat snake goos)
  (cond [(empty? goos) #f]
        [else (if (close? (snake-head snake) (first goos))
                  (first goos)
                  (can-eat snake (rest goos)))]))

;; Notice that we define our own posn comparator:
;; `posn=?`; again, using a specific comparator is encouraged
;; instead of just using `equal?`
(define (close? s g)
  (posn=? s (goo-loc g)))

(define (eat goos goo-to-eat)
  (cons (fresh-goo)
        (remove goo-to-eat goos)))

;; Notice how "growing" is really just prepending a new head
;; onto the existing snake--all the while creating a new snake.
(define (grow s)
  (snake (snake-dir s)
         (cons (next-head s)
               (snake-segs s))))

;; Similar to `grow`, slither creates a new snake with a new head,
;; but removes the last segment to keep the length and just change the
;; position.
(define (slither s)
  (snake (snake-dir s)
         (cons (next-head s)
               (all-but-last (snake-segs s)))))

;; notice how `empty` represents the empty list.
(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs)
                    (all-but-last (rest segs)))]))

;; Notice how in racket internal definitions are preferred over the use of
;; `let` and `let*`; this and other conventions are noted in:
;; https://docs.racket-lang.org/style/Choosing_the_Right_Construct.html#Definitions
(define (next-head s)
  (define head (snake-head s))
  (define dir  (snake-dir s))
  (cond [(string=? dir "up")   (posn-move head 0 -1)]
        [(string=? dir "down") (posn-move head 0 1)]
        [(string=? dir "left") (posn-move head -1 0)]
        [(string=? dir "right") (posn-move head 1 0)]))

(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

;; First, replace all rotten goo with fresh goo,
;; then, decay each of those. That way in each frame
;; we mimic the decay of all goos while ensure there's
;; always goo on the screen.
(define (age-goo goos)
  (rot (renew goos)))

(define (rot goos)
  (cond [(empty? goos) empty]
        [else (cons (decay (first goos))
                    (rot (rest goos)))]))

;; ERRATA: this function wasn't defined in the book,
;; had to copy it from the installed source.
(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire g))))


(define (renew goos)
  (cond [(empty? goos) empty]
        [(rotten? (first goos))
         (cons (fresh-goo) (renew (rest goos)))]
        [else
         (cons (first goos) (renew (rest goos)))]))

(define (rotten? goo)
  (zero? (goo-expire goo)))

(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       ;; The book doesn't randomize the expiration time,
       ;; but it sounded like fun to have goos with varying expirations.
       ;; notice that in all of these we need add1 to ensure
       ;; the random numbers are non-zero.
       (add1 (random EXPIRATION-TIME))))

;; PLAYER INPUT
(define (direct-snake w k)
  (cond [(dir? k) (world-change-dir w k)]
        [else w]))

(define (dir? k)
  (or (key=? k "up")
      (key=? k "down")
      (key=? k "left")
      (key=? k "right")))

(define (world-change-dir w d)
  (define the-snake (pit-snake w))
  ;; If you try to make the snake go in the opposite of its current dir
  ;; and the snake is more than just a head, we stop the game:
  ;; the snake would collide into itself.
  (cond [(and (opposite-dir? (snake-dir the-snake) d)
              (cons? (rest (snake-segs the-snake))))
         (stop-with w)]
        [else
         (pit (snake-change-dir the-snake d)
              (pit-goos w))]))

(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up")    (string=? d2 "down")]
        [(string=? d1 "down")  (string=? d2 "up")]
        [(string=? d1 "left")  (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]))

;; RENDERING

(define (render-pit w)
  (snake+scene (pit-snake w)
               (goo-list+scene (pit-goos w) MT-SCENE)))

(define (snake+scene snake scene)
  (define snake-body-scene
    (img-list+scene (snake-body snake) SEG-IMG scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake)
             (cond [(string=? "up" dir) HEAD-UP-IMG]
                   [(string=? "down" dir) HEAD-DOWN-IMG]
                   [(string=? "left" dir) HEAD-LEFT-IMG]
                   [(string=? "right" dir) HEAD-RIGHT-IMG])
             snake-body-scene))

(define (img-list+scene posns img scene)
  (cond [(empty? posns) scene]
        [else (img+scene
               (first posns)
               img
               (img-list+scene (rest posns) img scene))]))

;; `place-image` is provided by 2htdp/image
;; and places a given image in the given point of a scene.
(define (img+scene pos img scene)
  (place-image img
               (* (posn-x pos) SEG-SIZE)
               (* (posn-y pos) SEG-SIZE)
               scene))

(define (goo-list+scene goos scene)
  (define (get-posns-from-goo goos)
    (cond [(empty? goos) empty]
          [else (cons (goo-loc (first goos))
                      (get-posns-from-goo (rest goos)))]))
  (img-list+scene (get-posns-from-goo goos) GOO-IMG scene))

;; END GAME

(define (dead? w)
  (define snake (pit-snake w))
  (or (self-colliding? snake)
      (wall-colliding? snake)))

(define (self-colliding? snake)
  (cons? (member (snake-head snake)
                 (snake-body snake))))

(define (wall-colliding? snake)
  (define x (posn-x (snake-head snake)))
  (define y (posn-y (snake-head snake)))
  (or (= 0 x) (= x SIZE)
      (= 0 y) (= y SIZE)))

;; ERRATA: the book uses `render-snake-world` vs. `render-pit`
(define (render-end w)
  (overlay (text "Game Over!" ENDGAME-TEXT-SIZE "black")
           (render-pit w)))

;; UTILITY FUNCTIONS
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))


(start-snake)
