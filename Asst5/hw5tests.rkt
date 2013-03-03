#lang racket

(require "hw5.rkt")

;Simple int
(equal? (int 1) (eval-exp (int 1)))

;Add
(equal? (int 3) (eval-exp (add (int 1) (int 2))))

;ifgreater e1 > e2
(equal?
 (int 0) 
 (eval-exp (ifgreater (int 10) (int 9) (int 0) (int 1))))

;ifgreater e1 = e2
(equal?
 (int 1) 
 (eval-exp (ifgreater (int 10) (int 10) (int 0) (int 1))))

;ifgreater e1 < e2
(equal?
 (int 1) 
 (eval-exp (ifgreater (int 9) (int 10) (int 0) (int 1))))

;apair evaluates expression
(equal?
 (apair (int 3) (int 5))
 (eval-exp (apair (add (int 1) (int 2)) (int 5))))

;fst
(equal?
 (int 3)
 (eval-exp (fst
            (apair (add (int 1) (int 2)) (int 5)))))
;snd
(equal?
 (int 5)
 (eval-exp (snd
            (apair (add (int 1) (int 2)) (int 5)))))

;aunit
(equal? (aunit) (eval-exp (aunit)))

;isaunit
(equal? (int 1) (eval-exp (isaunit (aunit))))
(equal? (int 0) (eval-exp (isaunit (int 5))))

;mlet
(equal? (int 2) (eval-exp (mlet "a" (int 1) (add (int 1) (var "a")))))

; a test case that uses problems 1, 2, and 4
; should produce (list (int 10) (int 11) (int 16))
(define test1
  (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))))

