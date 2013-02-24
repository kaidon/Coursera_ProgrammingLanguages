#lang racket

(require "hw4.rkt") 

;; A simple library for displaying a 2x3 grid of pictures: used
;; for fun in the tests below (look for "Tests Start Here").

(require (lib "graphics.rkt" "graphics"))

(open-graphics)

(define window-name "Programming Languages, Homework 4")
(define window-width 700)
(define window-height 500)
(define border-size 100)

(define approx-pic-width 200)
(define approx-pic-height 200)
(define pic-grid-width 3)
(define pic-grid-height 2)

(define (open-window)
  (open-viewport window-name window-width window-height))

(define (grid-posn-to-posn grid-posn)
  (when (>= grid-posn (* pic-grid-height pic-grid-width))
    (error "picture grid does not have that many positions"))
  (let ([row (quotient grid-posn pic-grid-width)]
        [col (remainder grid-posn pic-grid-width)])
    (make-posn (+ border-size (* approx-pic-width col))
               (+ border-size (* approx-pic-height row)))))

(define (place-picture window filename grid-posn)
  (let ([posn (grid-posn-to-posn grid-posn)])
    ((clear-solid-rectangle window) posn approx-pic-width approx-pic-height)
    ((draw-pixmap window) filename posn)))

(define (place-repeatedly window pause stream n)
  (when (> n 0)
    (let* ([next (stream)]
           [filename (cdar next)]
           [grid-posn (caar next)]
           [stream (cdr next)])
      (place-picture window filename grid-posn)
      (sleep pause)
      (place-repeatedly window pause stream (- n 1)))))

;; Tests Start Here
;; Tests for 1.
(equal? (list 3 5 7 9 11) (sequence 3 11 2))
(equal? (list 3 6) (sequence 3 8 3))
(equal? null (sequence 3 2 1))

;; Tests for 2.
(equal? (list "abb" "bbb" "cbb") 
        (string-append-map (list "a" "b" "c") "bb"))
(equal? (list "a" "b" "c")
        (string-append-map (list "a" "b" "c") ""))

;; Tests for 3.
(equal? 1 (list-nth-mod (list 1 2 3 4 5) 20))
(equal? 5 (list-nth-mod (list 1 2 3 4 5) 19))
(equal? 4 (list-nth-mod (list 1 2 3 4 5) 18))
(equal? 3 (list-nth-mod (list 1 2 3 4 5) 17))
(equal? 2 (list-nth-mod (list 1 2 3 4 5) 16))
(equal? 1 (list-nth-mod (list 1 2 3 4 5) 15))

;; Tests for 4.
(define ones (lambda () (cons 1 ones)))
(equal? null (stream-for-n-steps ones 0))
(equal? (list 1) (stream-for-n-steps ones 1))
(equal? (list 1 1 1) (stream-for-n-steps ones 3))

;; Tests for 5.
(equal? (list 1 2 3 4 -5 6 7 8 9 -10) 
        (stream-for-n-steps funny-number-stream 10))

;; Tests for 6
(equal? (list "dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg")
        (stream-for-n-steps dan-then-dog 6))

;; Tests for 7
(equal? (list (cons 0 1) (cons 0 2) (cons 0 3))
        (stream-for-n-steps (stream-add-zero funny-number-stream) 3))

;; Tests for 8
(equal? (list (cons 1 "a") (cons 2 "b") (cons 3 "a") (cons 1 "b"))
  (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 4))

;; Tests for 9
(define testV (vector (cons 1 2) (cons 3 4) (cons 5 6)))
(equal? #f (vector-assoc 0 testV))
(equal? (cons 1 2) (vector-assoc 1 testV))
(equal? (cons 3 4) (vector-assoc 3 testV))
(equal? (cons 5 6) (vector-assoc 5 testV))
(equal? (cons "a" "b") (vector-assoc "a" (vector (cons "a" "b") (cons "b" "c"))))

; These definitions will work only after you do some of the problems
; so you need to comment them out until you are ready.
; Add more tests as appropriate, of course.

(define nums (sequence 0 5 1))


(define files (string-append-map 
               (list "dan" "dog" "curry" "dog2") 
               ".jpg"))

(define funny-test (stream-for-n-steps funny-number-stream 16))

; a zero-argument function: call (one-visual-test) to open the graphics window, etc.
(define (one-visual-test)
  (place-repeatedly (open-window) 0.5 (cycle-lists nums files) 27))

; similar to previous but uses only two files and one position on the grid
(define (visual-zero-only)
  (place-repeatedly (open-window) 0.5 (stream-add-zero dan-then-dog) 27))
