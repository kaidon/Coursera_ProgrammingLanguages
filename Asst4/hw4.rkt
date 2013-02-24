
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
  (if (equal? 0 n) 
      null
      (cons (car (s)) (stream-for-n-steps s (- n 1)))))