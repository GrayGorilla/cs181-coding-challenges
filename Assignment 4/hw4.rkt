#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below


;; 1
(define (sequence spacing low high)
  (cond [(= low high) (list low)]
        [(> low high) (list)]  ; empty list
        [#t (cons low (sequence spacing (+ low spacing) high))]))

;; 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (quote (error "list-nth-mod: negative number"))]
        [(= (length xs) 0) (quote (error "list-nth-mod: empty list"))]
        [#t (car (list-tail xs (remainder n (length xs))))]))