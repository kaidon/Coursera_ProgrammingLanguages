
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;1. sequence (low, high, stride)
(define (sequence low high stride)
  (if (>= high low)
      (cons low (sequence (+ low stride) high stride))
      null))

;2. string-append-map (xs, suffix)
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

;3. list-nth-mod (xs, n)
(define (list-nth-mod xs n)  
  (cond [(> 0 n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [(car (list-tail xs (remainder n (length xs))))]))

;4. stream-for-n-steps (s,n)
(define (stream-for-n-steps s n)
  (if (= 0 n) 
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;5. funny-number-stream
(define funny-number-stream
  (letrec ([f (lambda (x) 
                (cons 
                 (if (= (modulo x 5) 0) (- 0 x) x)
                 (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;6. dan-then-dog
(define dan-then-dog
  (letrec ([f (lambda (x) 
                (cons x (lambda () (f (if (equal? x "dan.jpg") "dog.jpg" "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

;7. stream-add-zero (s)
(define (stream-add-zero s)  
  (letrec ([f (lambda () 
                (cons (cons 0 (car (s))) (stream-add-zero (cdr (s)))))])
    (lambda () (f))))

;8. cycle-lists (xs,ys)
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) 
                (cons 
                 (cons (list-nth-mod xs n) (list-nth-mod ys n))                 
                 (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

;9. vector-assoc (v,vec)
(define (vector-assoc v vec)
  (letrec ([f (lambda (pos)
                (cond [(>= pos (vector-length vec)) #f]
                      [(not (pair? (vector-ref vec pos))) (f (+ 1 pos))]
                      [(equal? v (car (vector-ref vec pos))) (vector-ref vec pos)]
                      [#t (f (+ 1 pos))]
                      ))])
    (f 0)))

;10. cached-assoc (xs, n)
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [cacheIndex 0]
           [f (lambda (key)
                (let ([ans (vector-assoc key memo)])
                  (if ans          ;non-#f is #t!                      
                      ans    ;answer exists                  
                      (let ([new-ans (assoc key xs)])
                        (begin                               
                          (vector-set! memo cacheIndex new-ans)
                          (set! cacheIndex (modulo (+ 1 cacheIndex) n))
                          new-ans)))))] ;answer cached now
           )
    (lambda (x) (f x))))


