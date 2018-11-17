#lang racket

;;; Chapter 5 of Realm of Racket: the guess my number UI
;;; Full source from authors available at:
;;; "/Applications/Racket v7.1/share/pkgs/realm/chapter5/source.rkt"

(require 2htdp/universe 2htdp/image)


;;; CONSTANTS

(define TEXT-SIZE 11)
(define HELP-TEXT
  ;; `text` creates an image from text
  (text "↑ for larger numbers, ↓ for smaller ones" 
        TEXT-SIZE 
        "blue"))
(define HELP-TEXT2 
  (text "Press = when your number is guessed; q to quit." 
        TEXT-SIZE 
        "blue"))
(define WIDTH (+ (image-width HELP-TEXT2) 10))
(define HEIGHT 150)
(define COLOR "red")
(define SIZE 72)
(define TEXT-X 3)
(define TEXT-UPPER-Y 10)
(define TEXT-LOWER-Y 135)
;; Background scene:
(define MT-SC
  (place-image/align
   HELP-TEXT TEXT-X TEXT-UPPER-Y "left" "top"
   (place-image/align
    HELP-TEXT2 TEXT-X TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))

;; Racketeers seem to prefer structs to maps, which is a bit backwards IMO
;; but structs are immutable too, so I see the parallels in preference.
(struct interval (small big))

;;; BEHAVIOR

(define (start lower upper)
  (big-bang (interval lower upper)
            (on-key deal-with-guess)
            (to-draw render)
            (stop-when single? render-last-scene)))

#|

`key=?` is provided by 2htdp/universe to compare key-events specifically, which are a subset of strings. (so `string=?` could work too, but in Racket hinting at the types when comparing is encouraged).

`stop-with` is also provided by 2htdp/universe and it will cause rendering to stop with the given state; it causes the handler given to `stop-when` to be called, if provided; otherwise on-draw is called.
|#

(define (deal-with-guess w k)
  (cond [(key=? k "up")   (bigger w)]
        [(key=? k "down") (smaller w)]
        [(key=? k "q")    (stop-with w)]
        [(key=? k "=")    (stop-with w)]
        [else w]))

;; Notice how all these functions operate on a given w (world state)
;; instead of dealing with mutable globals.

(define (smaller w)
  (interval (interval-small w)
            (max (interval-small w) (sub1 (guess w)))))

(define (bigger w)
  (interval (min (interval-big w) (add1 (guess w)))
            (interval-big w)))

(define (guess w)
  (quotient (+ (interval-small w)
               (interval-big w))
            2))

(define (render w)
  (overlay (text (number->string (guess w))
                 SIZE COLOR)
           MT-SC))

(define (render-last-scene w)
  (overlay (text "End" SIZE COLOR)
           MT-SC))

(define (single? w)
  (= (interval-small w)
     (interval-big w)))

;; On Emacs, do `C-c C-k` to run it, you should see a little window with
;; the guessing game pop up.
(start 10 50)
