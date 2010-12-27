;this is part of lecture 3-a
;remember
;1. primitives
;2. means of combination
;3. means of abstraction
;building DSLs -> metalinguistic abstraction

;PRIMITIVES: 
; picture: a drawing enclosed in a rectangle

;MEANS OF COMBINATION:
; rotate: given an angle and a rectangle, rotate the picture
; flip: horizontally or vertically
; beside: two pictures and a number `s` in [0,1], put both in the same rectangle
;         with s delimiting how much (percentage) the first picture takes horizontally
; above: takes two pictures and ``s`` and puts 'em one above the other, with s delimiting 
;        the percentage the first takes vertically

;the cool thing is CLOSURE: the combination of two pictures is itself a picture and so forth, 
; "the world of pictures is closed under those means of combination"

;the basic thing, the rectangle
(define (make-rect origin horiz vert)
    (cons origin (cons horiz vert)))

(define (origin rect)
    (car rect))

(define (horiz rect)
    (cadr rect))

(define (vert rect)
    (cddr rect))

;scaling can be seen as taking something enclosed in a "unit square" (whose origin is (0,0) and side length is 1)
;and transposing is points in a uniform manner in a rectangle:
;(x,y) -> origin + x . horiz + y . vert
;coord-map takes a rectangle and returns a procedure on points:
;transforms a regular point to a point in the rectangle
(define (coord-map rect)
    (lambda (point)
        (+vect 
            (+vect (scale (xcor point)
                          (horiz rect))
                   (scale (ycor point)
                          (vert rect)))
            (origin rect))))

;PICTURE primitive: given a list of vectors, return a procedure that'll draw it in a rectangle :O
(define (make-picture seglist)
    (lambda (rect)
        (for-each 
            (lambda (s)
                (drawline 
                    ((coord-map rect) (seg-start s))
                    ((coord-map rect) (seg-end s))))
            seglist)))

;implementing means of combination
;beside takes two pictures and draws them in two derived rectangles; which will draw a new picture in a new rectangle
(define (beside p1 p2 a)
    (lambda (rect)
        (p1 (make-rect
                (origin rect)
                (scale a (horiz rect))
                (vert rect)))
        (p2 (make-rect
                (+vect (origin rect)
                       (scale a (horiz rect)))
                (scale (- 1 a) (horiz rect))
                (vert rect)))))

(define (rotate90 pict)
    (lambda (rect)
        (make-picture (make-rect
                        (+vect (origin rect)
                               (horiz rect))
                        (vert rect)
                        (scale -1 (horiz rect))))))

;implementing pictures as procedures ensures closure
;and also lets us take advantage of lisp's power on procedures: that is
; *embedding* something in a language instead of *implementing*: 
; embedding lets us keep the original language's power

;this blurs the line between procedures and data :O

;also, an argument against structured software engineering against "the lisp way":
;abstractly building languages

                


