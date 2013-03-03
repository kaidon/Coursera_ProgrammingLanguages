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


;fun recurive with call
(equal? (int 11) (eval-exp 
 (mlet "func"
       (fun "f1" "x"
            (ifgreater (var "x") (int 10)
                       (var "x")
                       (begin                          
                         (call (var "f1") (add (var "x") (int 1))))))
       (call (var "func") (int 0)))))

;fun uses closures
(equal? (int 3)
(eval-exp
 (mlet "a" (int 1)
       (mlet "f" (fun #f "x" (add (var "a") (int 2)))
             (mlet "a" (int 20) (call (var "f") (int 0)))))))             

;ifaunit
(equal? 
 (ifgreater (isaunit (int 1)) (int 0) (int 2) (int 3))
 (ifaunit (int 1) (int 2) (int 3)))

;mlet*
(equal?
 (mlet "a" (int 1) (mlet "b" (int 2) (int 3)))
 (mlet* (list (cons "a" (int 1)) (cons "b" (int 2))) (int 3)))

;ifeq
(equal?
 (int 10) 
 (eval-exp (ifeq (int 1) (int 2) (int 0) (int 10))))

(equal?
 (int 0) 
 (eval-exp (ifeq (int 2) (int 2) (int 0) (int 10))))
               
; a test case that uses problems 1, 2, and 4
; should produce (list (int 10) (int 11) (int 16))
(define test1
  (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))))

